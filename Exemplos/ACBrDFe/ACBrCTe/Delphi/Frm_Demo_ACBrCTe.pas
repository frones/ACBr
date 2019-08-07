{$I ACBr.inc}

unit Frm_Demo_ACBrCTe;

interface

uses
  IniFiles, ShellAPI,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, OleCtrls, SHDocVw, StdCtrls, Buttons, ExtCtrls,
  ACBrCTe, ACBrCTeDACTEClass, ACBrMail, ACBrBase, ACBrDFe, ACBrCTeDACTeRLClass, ACBrDFeSSL,
  ACBrDFeReport;

type
  TfrmDemo_ACBrCTe = class(TForm)
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label25: TLabel;
    sbtnGetCert: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    TabSheet2: TTabSheet;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    sbtnPathSalvar: TSpeedButton;
    edtLogoMarca: TEdit;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    rgTipoDACTe: TRadioGroup;
    rgFormaEmissao: TRadioGroup;
    TabSheet3: TTabSheet;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    ckVisualizar: TCheckBox;
    cbUF: TComboBox;
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
    TabSheet7: TTabSheet;
    GroupBox5: TGroupBox;
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
    btnConsultar: TButton;
    btnValidarXML: TButton;
    btnStatusServ: TButton;
    btnCancCTe: TButton;
    btnCriarEnviar: TButton;
    btnInutilizar: TButton;
    btnGerarCTe: TButton;
    btnConsCad: TButton;
    btnGerarPDF: TButton;
    btnEnviarEmail: TButton;
    btnConsultarRecibo: TButton;
    btnEnvEPEC: TButton;
    btnImprimirEvento: TButton;
    btnConsultarChave: TButton;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TWebBrowser;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwCTe: TTreeView;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    OpenDialog1: TOpenDialog;
    ACBrCTe1: TACBrCTe;
    btnEnviarEventoEmail: TButton;
    btnGerarPDFEvento: TButton;
    btnImprimirInut: TButton;
    btnGerarPDFInut: TButton;
    ACBrMail1: TACBrMail;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    lSSLLib: TLabel;
    cbSSLLib: TComboBox;
    lCryptLib: TLabel;
    cbCryptLib: TComboBox;
    lHttpLib: TLabel;
    cbHttpLib: TComboBox;
    lXmlSign: TLabel;
    cbXmlSignLib: TComboBox;
    lSSLLib1: TLabel;
    cbSSLType: TComboBox;
    rgModeloDF: TRadioGroup;
    rgVersaoDF: TRadioGroup;
    btnValidarAssinatura: TButton;
    btnCriarEnviarSincrono: TButton;
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure btnStatusServClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarChaveClick(Sender: TObject);
    procedure btnCancCTeClick(Sender: TObject);
    procedure btnInutilizarClick(Sender: TObject);
    procedure btnConsultarReciboClick(Sender: TObject);
    procedure btnConsCadClick(Sender: TObject);
    procedure btnGerarCTeClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnEnvEPECClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure ACBrCTe1StatusChange(Sender: TObject);
    procedure ACBrCTe1GerarLog(const Mensagem: String);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
    procedure btnGerarPDFEventoClick(Sender: TObject);
    procedure btnImprimirInutClick(Sender: TObject);
    procedure btnGerarPDFInutClick(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure btnCriarEnviarSincronoClick(Sender: TObject);
    {
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    }
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TWebBrowser);
    procedure GerarCTe(NumCTe : String);
    procedure GerarCTeOS(NumCTe : String);
    procedure AtualizaSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmDemo_ACBrCTe: TfrmDemo_ACBrCTe;

implementation

uses
  FileCtrl, DateUtils,
  ufrmStatus,
  pcnConversao, pcteConversaoCTe, ACBrUtil,
  ACBrCTeConhecimentos, blcksock, TypInfo;

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
procedure TfrmDemo_ACBrCTe.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
    Ini.WriteInteger( 'Certificado','SSLLib' , cbSSLLib.ItemIndex) ;
    Ini.WriteInteger( 'Certificado','CryptLib' , cbCryptLib.ItemIndex) ;
    Ini.WriteInteger( 'Certificado','HttpLib' , cbHttpLib.ItemIndex) ;
    Ini.WriteInteger( 'Certificado','XmlSignLib' , cbXmlSignLib.ItemIndex) ;
    Ini.WriteString( 'Certificado','Caminho' ,edtCaminho.Text);
    Ini.WriteString( 'Certificado','Senha'   ,edtSenha.Text);
    Ini.WriteString( 'Certificado','NumSerie',edtNumSerie.Text);

    Ini.WriteInteger( 'Geral','DACTE'       ,rgTipoDACTe.ItemIndex);
    Ini.WriteInteger( 'Geral','FormaEmissao',rgFormaEmissao.ItemIndex);
    Ini.WriteString( 'Geral','LogoMarca'   ,edtLogoMarca.Text);
    Ini.WriteBool(   'Geral','Salvar'      ,ckSalvar.Checked);
    Ini.WriteString( 'Geral','PathSalvar'  ,edtPathLogs.Text);
    Ini.WriteInteger( 'Geral','ModeloDF',rgModeloDF.ItemIndex);
    Ini.WriteInteger( 'Geral','VersaoDF',rgVersaoDF.ItemIndex);

    Ini.WriteString( 'WebService','UF'        ,cbUF.Text);
    Ini.WriteInteger( 'WebService','Ambiente'  ,rgTipoAmb.ItemIndex);
    Ini.WriteBool(   'WebService','Visualizar',ckVisualizar.Checked);
    Ini.WriteInteger( 'WebService','SSLType' , cbSSLType.ItemIndex) ;

    Ini.WriteString( 'Proxy','Host'   ,edtProxyHost.Text);
    Ini.WriteString( 'Proxy','Porta'  ,edtProxyPorta.Text);
    Ini.WriteString( 'Proxy','User'   ,edtProxyUser.Text);
    Ini.WriteString( 'Proxy','Pass'   ,edtProxySenha.Text);

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
  finally
    Ini.Free;
  end;
end;

procedure TfrmDemo_ACBrCTe.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  Ok: Boolean;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
    cbSSLLib.ItemIndex     := Ini.ReadInteger( 'Certificado','SSLLib' ,0) ;
    cbCryptLib.ItemIndex   := Ini.ReadInteger( 'Certificado','CryptLib' , 0) ;
    cbHttpLib.ItemIndex    := Ini.ReadInteger( 'Certificado','HttpLib' , 0) ;
    cbXmlSignLib.ItemIndex := Ini.ReadInteger( 'Certificado','XmlSignLib' , 0) ;
    edtCaminho.Text        := Ini.ReadString( 'Certificado','Caminho','') ;
    edtSenha.Text          := Ini.ReadString( 'Certificado','Senha','') ;
    edtNumSerie.Text       := Ini.ReadString( 'Certificado','NumSerie','') ;

    ACBrCTe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
    ACBrCTe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
    ACBrCTe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

    rgFormaEmissao.ItemIndex := Ini.ReadInteger('Geral','FormaEmissao',0);
    ckSalvar.Checked         := Ini.ReadBool(   'Geral','Salvar'      ,True);
    edtPathLogs.Text         := Ini.ReadString( 'Geral','PathSalvar'  ,'');
    rgModeloDF.ItemIndex     := Ini.ReadInteger('Geral', 'ModeloDF', 0);
    rgVersaoDF.ItemIndex     := Ini.ReadInteger('Geral', 'VersaoDF', 0);

    ACBrCTe1.SSL.DescarregarCertificado;

    with ACBrCTe1.Configuracoes.Geral do
    begin
      if rgModeloDF.ItemIndex = 0 then
        ModeloDF := moCTe
      else
        ModeloDF := moCTeOS;

      if rgVersaoDF.ItemIndex =0 then
        VersaoDF := ve200
      else
        VersaoDF := ve300;

      SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

      AtualizaSSLLibsCombo;

      case rgFormaEmissao.ItemIndex of
        0: FormaEmissao := teNormal;
        1: FormaEmissao := teDPEC; // o mesmo que EPEC
        2: FormaEmissao := teFSDA;
        3: FormaEmissao := teSVCRS;
        4: FormaEmissao := tESVCSP;
      end;

      Salvar := ckSalvar.Checked;
    end;

    ACBrCTe1.Configuracoes.Arquivos.PathSalvar := edtPathLogs.Text;

    cbUF.ItemIndex       := cbUF.Items.IndexOf(Ini.ReadString('WebService','UF','SP'));
    rgTipoAmb.ItemIndex  := Ini.ReadInteger('WebService','Ambiente'  ,0);
    ckVisualizar.Checked := Ini.ReadBool(   'WebService','Visualizar',False);
    cbSSLType.ItemIndex  := Ini.ReadInteger('WebService','SSLType' , 0) ;

    ACBrCTe1.Configuracoes.WebServices.UF         := cbUF.Text;
    ACBrCTe1.Configuracoes.WebServices.Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    ACBrCTe1.Configuracoes.WebServices.Visualizar := ckVisualizar.Checked;

    edtProxyHost.Text  := Ini.ReadString( 'Proxy','Host'   ,'');
    edtProxyPorta.Text := Ini.ReadString( 'Proxy','Porta'  ,'');
    edtProxyUser.Text  := Ini.ReadString( 'Proxy','User'   ,'');
    edtProxySenha.Text := Ini.ReadString( 'Proxy','Pass'   ,'');

    ACBrCTe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
    ACBrCTe1.Configuracoes.WebServices.ProxyPort := edtProxyPorta.Text;
    ACBrCTe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
    ACBrCTe1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;

    ACBrCTe1.SSL.SSLType := TSSLType( cbSSLType.ItemIndex );

    rgTipoDACTe.ItemIndex := Ini.ReadInteger( 'Geral','DACTE'       ,0);
    edtLogoMarca.Text     := Ini.ReadString( 'Geral','LogoMarca'   ,'');

    if ACBrCTe1.DACTe <> nil then
    begin
      ACBrCTe1.DACTe.TipoDACTe    := StrToTpImp(OK,IntToStr(rgTipoDaCTe.ItemIndex+1));
      ACBrCTe1.DACTe.Logo         := edtLogoMarca.Text;
      ACBrCTe1.DACTe.PathPDF      := edtPathLogs.Text;
      ACBrCTe1.DACTe.TamanhoPapel := tpA4_2vias;
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
    edtEmitCidade.Text     := Ini.ReadString( 'Emitente','Cidade'     ,'');
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

    ACBrCTe1.MAIL.Host := edtSmtpHost.Text;
    ACBrCTe1.MAIL.Port := edtSmtpPort.Text;
    ACBrCTe1.MAIL.Username := edtSmtpUser.Text;
    ACBrCTe1.MAIL.Password := edtSmtpPass.Text;
    ACBrCTe1.MAIL.SetSSL   := cbEmailSSL.Checked;
    ACBrCTe1.MAIL.ReadingConfirmation := False;

  finally
    Ini.Free;
  end;
end;

procedure TfrmDemo_ACBrCTe.LoadXML(MyMemo: TMemo; MyWebBrowser: TWebBrowser);
begin
  MyMemo.Lines.SaveToFile(ExtractFileDir(application.ExeName)+'temp.xml');
  MyWebBrowser.Navigate(ExtractFileDir(application.ExeName)+'temp.xml');
end;

procedure TfrmDemo_ACBrCTe.GerarCTe(NumCTe: String);
begin
  //CTe
  with ACBrCTe1.Conhecimentos.Add.CTe do
  begin
    if rgVersaoDF.ItemIndex = 0 then
      infCTe.versao := 2.0
    else
      infCTe.versao := 3.0;

    Ide.cUF    := UFtoCUF(edtEmitUF.Text);
    // Atenção o valor de cCT tem que ser um numero aleatório conforme recomendação
    // da SEFAZ, mas neste exemplo vamos atribuir o mesmo numero do CT-e.
    Ide.cCT    := 1; // Tem que ser um numero aleatório
    Ide.CFOP   := 5353;
    Ide.natOp  := 'PRESTACAO SERVICO';
    ide.forPag := fpAPagar; // fpAPagar ou fpPago
    Ide.modelo := 57;
    Ide.serie  := 1;
    Ide.nCT    := StrToInt(NumCTe);
    Ide.dhEmi  := Now;
    Ide.tpImp  := tiRetrato;

    case rgFormaEmissao.ItemIndex of
      0: Ide.tpEmis := teNormal;
      1: Ide.tpEmis := teDPEC;
      2: Ide.tpEmis := teFSDA;
      3: Ide.tpEmis := teSVCRS;
      4: Ide.tpEmis := teSVCSP;
    end;

    if rgTipoAmb.ItemIndex = 0 then
      Ide.tpAmb := taProducao
    else
      Ide.tpAmb := taHomologacao;

    Ide.tpCTe      := tcNormal; // tcNormal, tcComplemento, tcAnulacao, tcSubstituto
    Ide.procEmi    := peAplicativoContribuinte;
    Ide.verProc    := '3.0';
    Ide.cMunEnv    := StrToInt(edtEmitCodCidade.Text);
    Ide.xMunEnv    := Trim(edtEmitCidade.Text);
    Ide.UFEnv      := Trim(edtEmitUF.Text);
    Ide.modal      := mdRodoviario;
    Ide.tpServ     := tsNormal; // tsNormal, tsSubcontratacao, tsRedespacho, tsIntermediario
    ide.indIEToma  := inContribuinte;
    Ide.cMunIni    := 3119401;
    Ide.xMunIni    := 'CORONEL FABRICIANO';
    Ide.UFIni      := 'MG';
    Ide.cMunFim    := 2900207;
    Ide.xMunFim    := 'ABARE';
    Ide.UFFim      := 'BA';
    Ide.retira     := rtSim; // rtSim, rtNao
    Ide.xdetretira := '';

    ide.indGlobalizado := tiNao;

    {Dados do Percurso}
    (*
    with ide.infPercurso.Add do
      UFPer := 'PR';
    *)

    Ide.Toma03.Toma := tmRemetente; // tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmRemetente

    {Dados do Tomador: Outros}
    Ide.Toma4.Toma    := tmOutros;
    Ide.Toma4.CNPJCPF := '10242141000174';
    Ide.Toma4.IE      := '0010834420031';
    Ide.Toma4.xNome   := 'ACOUGUE E SUPERMERCADO SOUZA LTDA';
    Ide.Toma4.xFant   := '';
    Ide.Toma4.fone    := '';

    Ide.Toma4.enderToma.xLgr    := 'RUA BELO HORIZONTE';
    Ide.Toma4.enderToma.nro     := '614';
    Ide.Toma4.enderToma.xCpl    := 'N D';
    Ide.Toma4.enderToma.xBairro := 'CALADINA';
    Ide.Toma4.enderToma.cMun    := 3119401;
    Ide.Toma4.enderToma.xMun    := 'CORONEL FABRICIANO';
    Ide.Toma4.enderToma.CEP     := 35171167;
    Ide.Toma4.enderToma.UF      := 'MG';
    Ide.Toma4.enderToma.cPais   := 1058;
    Ide.Toma4.enderToma.xPais   := 'BRASIL';
    Ide.Toma4.email             := '';

    {Informações Complementares do CTe}
    compl.xCaracAd  := 'Caracteristicas Adicionais do Transporte';
    compl.xCaracSer := 'Caracteristicas Adicionais do Serviço';
    compl.xEmi      := 'Nome do Emitente';

    // Descricao da Origiem do Fluxo
    compl.fluxo.xOrig := '';

    (*
    with compl.fluxo.pass.Add do
    begin
      xPass := 'Sigla ou código interno da Filial/Porto/Estação/Aeroporto de Passagem ';
    end;

    compl.fluxo.xDest := 'Destino';
    compl.fluxo.xRota := 'Rota';
    *)

    compl.Entrega.TipoData      := tdSemData;
    compl.Entrega.semData.tpPer := tdSemData;
    // se tdNaData ou tdAteData ou tdApartirData
//    compl.Entrega.comData.tpPer := tdNaData
//    compl.Entrega.comData.dProg := Date;
    // se tdNoPeriodo
//    compl.Entrega.noPeriodo.tpPer := tdNoPeriodo;
//    compl.Entrega.noPeriodo.dIni  := Date;
//    compl.Entrega.noPeriodo.dFim  := Date + 5;

    compl.Entrega.TipoHora      := thSemHorario;
    compl.Entrega.semHora.tpHor := thSemHorario;
    // se thNoHorario ou thAteHorario ou thApartirHorario
//    compl.Entrega.comHora.tpHor := thNoHorario;
//    compl.Entrega.comHora.hProg := Time;
    // se thNoIntervalo
    compl.Entrega.noInter.tpHor := thNoIntervalo;
    compl.Entrega.noInter.hIni  := Time;
    compl.Entrega.noInter.hFim  := Time + 60;

    compl.origCalc := 'Município de origem para efeito de cálculo do frete ';
    compl.destCalc := 'Município de destino para efeito de cálculo do frete ';
    compl.xObs     := 'Observação livre';

    // Obs Estruturada do Contribuinte - Incluir se necessário
    with compl.ObsCont.New do
    begin
      xCampo := 'Nome do Campo';
      xTexto := 'Valor do Campo';
    end;

    // Obs Estruturada para o Fisco - Incluir se necessário
    with compl.ObsFisco.New do
    begin
      xCampo := 'Nome do Campo';
      xTexto := 'Valor do Campo';
    end;

    {Dados do Emitente}
    Emit.CNPJ              := Trim(edtEmitCNPJ.Text);
    Emit.IE                := Trim(edtEmitIE.Text);
    Emit.xNome             := Trim(edtEmitRazao.Text);
    Emit.xFant             := Trim(edtEmitFantasia.Text);
    Emit.enderEmit.xLgr    := Trim(edtEmitLogradouro.Text);
    Emit.enderEmit.nro     := Trim(edtEmitNumero.Text);
    Emit.enderEmit.xCpl    := Trim(edtEmitComp.Text);
    Emit.enderEmit.xBairro := Trim(edtEmitBairro.Text);
    Emit.enderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.enderEmit.xMun    := Trim(edtEmitCidade.Text);
    Emit.enderEmit.CEP     := StrToInt(edtEmitCEP.Text);
    Emit.enderEmit.UF      := Trim(edtEmitUF.Text);
    Emit.enderEmit.fone    := Trim(edtEmitFone.Text);

    {Dados do Remetente}
    Rem.CNPJCPF := '12345678000123';
    Rem.IE      := '12345678';
    Rem.xNome   := 'Nome do Remetente';
    Rem.xFant   := 'Nome Fantasia';
    Rem.fone    := '33445566';

    Rem.EnderReme.xLgr    := 'Rua 1';
    Rem.EnderReme.nro     := '200';
    Rem.EnderReme.xCpl    := '';
    Rem.EnderReme.xBairro := 'Centro';
    Rem.EnderReme.cMun    := 3512345;
    Rem.EnderReme.xMun    := 'Nome do Municipio';
    Rem.EnderReme.CEP     := 14123456;
    Rem.EnderReme.UF      := 'SP';
    Rem.EnderReme.cPais   := 1058;
    Rem.EnderReme.xPais   := 'BRASIL';

    {Dados do Expedidor - Utilizado no Redespacho Intermediário}
    (*
    Exped.CNPJCPF := '12345678000123';
    Exped.IE      := '12345678';
    Exped.xNome   := 'Nome do Expedidor';
    Exped.fone    := '33445566';

    Exped.EnderExped.xLgr    := 'Rua 1';
    Exped.EnderExped.nro     := '200';
    Exped.EnderExped.xCpl    := '';
    Exped.EnderExped.xBairro := 'Centro';
    Exped.EnderExped.cMun    := 3512345;
    Exped.EnderExped.xMun    := 'Nome do Municipio';
    Exped.EnderExped.CEP     := 14123456;
    Exped.EnderExped.UF      := 'SP';
    Exped.EnderExped.cPais   := 1058;
    Exped.EnderExped.xPais   := 'BRASIL';
    *)

    {Dados do Recebedor - Utilizado no Redespacho e Redespacho Intermediário}
    (*
    Receb.CNPJCPF := '12345678000123';
    Receb.IE      := '12345678';
    Receb.xNome   := 'Nome do Recebedor';
    Receb.fone    := '33445566';

    Receb.EnderReceb.xLgr    := 'Rua 1';
    Receb.EnderReceb.nro     := '200';
    Receb.EnderReceb.xCpl    := '';
    Receb.EnderReceb.xBairro := 'Centro';
    Receb.EnderReceb.cMun    := 3512345;
    Receb.EnderReceb.xMun    := 'Nome do Municipio';
    Receb.EnderReceb.CEP     := 14123456;
    Receb.EnderReceb.UF      := 'SP';
    Receb.EnderReceb.cPais   := 1058;
    Receb.EnderReceb.xPais   := 'BRASIL';
    *)

    {Dados do Destinatário}
    Dest.CNPJCPF := '12345678000123';
    Dest.IE      := '12345678';
    Dest.xNome   := 'Nome do Destinatário';
    Dest.fone    := '33445566';

    Dest.EnderDest.xLgr    := 'Rua 1';
    Dest.EnderDest.nro     := '200';
    Dest.EnderDest.xCpl    := '';
    Dest.EnderDest.xBairro := 'Centro';
    Dest.EnderDest.cMun    := 3512345;
    Dest.EnderDest.xMun    := 'Nome do Municipio';
    Dest.EnderDest.CEP     := 14123456;
    Dest.EnderDest.UF      := 'SP';
    Dest.EnderDest.cPais   := 1058;
    Dest.EnderDest.xPais   := 'BRASIL';

    {Carrega valores da prestacao de servico}
    vPrest.vTPrest := 100.00;
    vPrest.vRec    := 100.00;

    {Carrega componentes do valor da prestacao}
    with vPrest.comp.New do
    begin
      xNome := 'DFRNER KRTJ';
      vComp := 100.00;
    end;

    {Carrega Impostos}
    //00 - Tributação Normal ICMS
    {Imp.ICMS.SituTrib    := cst00;
    Imp.ICMS.ICMS00.CST  := cst00;
    Imp.ICMS.ICMS00.vBC  := Impostos.Vbc;
    Imp.ICMS.ICMS00.pICMS:= Impostos.Picms;
    Imp.ICMS.ICMS00.vICMS:= Impostos.Vicms;

    //40 - ICMS Isento
    Imp.ICMS.SituTrib  := cst40;
    Imp.ICMS.ICMS45.CST:= cst40;

    //41 - ICMS não Tributada
    Imp.ICMS.SituTrib  := cst41;
    Imp.ICMS.ICMS45.CST:= cst41;

    //51 - ICMS diferido
    Imp.ICMS.SituTrib  := cst51;
    Imp.ICMS.ICMS45.CST:= cst51; }

    //90 - ICMS Outros
    if Emit.enderEmit.UF = Rem.enderReme.UF then
    begin
      Imp.ICMS.SituTrib     := cst90;
      Imp.ICMS.ICMS90.CST   := cst90;
      Imp.ICMS.ICMS90.pRedBC:= 10.00;
      Imp.ICMS.ICMS90.vBC   := 100.00;
      Imp.ICMS.ICMS90.pICMS := 7.00;
      Imp.ICMS.ICMS90.vICMS := 6.30;
      Imp.ICMS.ICMS90.vCred := 0.00;
    end
    else
    begin
      Imp.ICMS.SituTrib                  := cstICMSOutraUF;
      Imp.ICMS.ICMSOutraUF.CST           := cstICMSOutraUF; // ICMS Outros
      Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := 0;
      Imp.ICMS.ICMSOutraUF.vBCOutraUF    := 100.00;
      Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := 7.00;
      Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := 7.00;
    end;

    //SN - Simples Nacional
    {Imp.ICMS.SituTrib     := cstICMSSN;
    Imp.ICMS.ICMSSN.indSN := 1;}

    Imp.infAdFisco := 'Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preço deste servico é de R$ 17,00 (17,00%) Fonte: IBPT';
    imp.vTotTrib   := 17.00;

    {Carrega as informacoes CTe Normal}
    with infCTeNorm do
    begin
      {Informações da Carga}
      infCarga.vCarga      := 5000;
      infCarga.proPred     := 'Produto Predominante';
      infCarga.xOutCat     := 'Outras Caractereisticas da Carga';
      infCarga.vCargaAverb := 5000;

      // UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS);
      with infCarga.InfQ.New do
      begin
        cUnid  := uKg;
        tpMed  := 'Kg';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uUnidade;
        tpMed  := 'Caixa';
        qCarga := 5;
      end;

      {Informações dos Documentos}
      with infDoc.infNFe.New do
        chave := 'chave da NFe emitida pelo remente da carga';

      {Carrega Informacoes do Modal}
      {Rodoviario}
      rodo.RNTRC := '12345678';

      {Ordens de Coleta associados}
      (*
      with rodo.occ.Add do
      begin
        serie := '001';
        nOcc  := 1;
        dEmi  := Date;

        emiOcc.CNPJ := '12345678000123';
        emiOcc.cInt := '501';
        emiOcc.IE   := '1234567';
        emiOcc.UF   := 'SP';
        emiOcc.fone := '22334455';
      end;
      *)

      {Carrega dados da CTe substituta 0-1}
      {with infCTeSub do
      begin
        chCte := '';
        //Se tomador não é Contribuinte
          tomaNaoICMS.refCteAnu := '';

        //Se tomador for Contribuinte
          case TipoDoc of //Tipo do Documento que o Tomador Emitiu para anulação de valor do Cte Anterior
            0: tomaICMS.refNFe := '';//NFe
            1: tomaICMS.refCte := '';//CTe
            2://NF
            begin
              tomaICMS.refNF.CNPJCPF  := '';
              tomaICMS.refNF.modelo   := '';
              tomaICMS.refNF.serie    := 0;
              tomaICMS.refNF.subserie := 0;
              tomaICMS.refNF.nro      := 0;
              tomaICMS.refNF.valor    := 0;
              tomaICMS.refNF.dEmi     := Date;
            end;
          end;
      end;}

      with cobr do
      begin
        fat.nFat  := '123';
        fat.vOrig := 100;
        fat.vDesc := 0;
        fat.vLiq  := 100;

        with dup.New do
        begin
          nDup  := '123';
          dVenc := Date + 30;
          vDup  := 100;
        end;
      end;
    end;

    {CTe de Complemento de valor}
      //InfCTeComp.chave := '';

    {CTe de Anulacao de valores}
      //infCteAnu.chCTe := '';
      //infCteAnu.dEmi  := Date;

    {Seleciona o dados dos Autorizados a baixar o xml}
      //autXML.Add.CNPJCPF := '';
  end;
end;

procedure TfrmDemo_ACBrCTe.GerarCTeOS(NumCTe: String);
begin
  //CTeOS
  with ACBrCTe1.Conhecimentos.Add.CTe do
  begin
    if rgVersaoDF.ItemIndex = 0 then
      infCTe.versao := 2.0
    else
      infCTe.versao := 3.0;

    Ide.cUF         := UFtoCUF(edtEmitUF.Text);
    Ide.cCT         := StrToInt(NumCTe);
    Ide.CFOP        := 6932;
    Ide.natOp       := 'PRESTACAO SERVICO TRANSPORTE INICIO OUTRA UF FORA DO ESTADO';
    Ide.modelo      := 67;
    Ide.serie       := 32;
    Ide.nCT         := StrToInt(NumCTe);
    Ide.dhEmi       := Now;
    Ide.tpImp       := tiRetrato;

    case rgFormaEmissao.ItemIndex of
      0: Ide.tpEmis := teNormal;
      1: Ide.tpEmis := teDPEC;
      2: Ide.tpEmis := teFSDA;
      3: Ide.tpEmis := teSVCRS;
      4: Ide.tpEmis := teSVCSP;
    end;

    if rgTipoAmb.ItemIndex = 0 then
      Ide.tpAmb := taProducao
    else
      Ide.tpAmb := taHomologacao;

    Ide.tpCTe     := tcNormal;
    Ide.procEmi   := peAplicativoContribuinte;
    Ide.verProc   := '3.0';
    Ide.cMunEnv   := StrToInt(edtEmitCodCidade.Text);
    Ide.xMunEnv   := Trim(edtEmitCidade.Text);
    Ide.UFEnv     := Trim(edtEmitUF.Text);
    Ide.modal     := mdRodoviario;
    Ide.tpServ    := tsTranspValores;
    ide.indIEToma := inContribuinte;
    Ide.cMunIni   := 3119401;
    Ide.xMunIni   := 'CORONEL FABRICIANO';
    Ide.UFIni     := 'MG';
    Ide.cMunFim   := 2900207;
    Ide.xMunFim   := 'ABARE';
    Ide.UFFim     := 'BA';

    {Dados do Percurso}
    //ide.infPercurso.Add.UFPer := 'PR';

    {Dados do Emitente}
    Emit.CNPJ              := Trim(edtEmitCNPJ.Text);
    Emit.IE                := Trim(edtEmitIE.Text);
    Emit.xNome             := Trim(edtEmitRazao.Text);
    Emit.xFant             := Trim(edtEmitFantasia.Text);
    Emit.enderEmit.xLgr    := Trim(edtEmitLogradouro.Text);
    Emit.enderEmit.nro     := Trim(edtEmitNumero.Text);
    Emit.enderEmit.xCpl    := Trim(edtEmitComp.Text);
    Emit.enderEmit.xBairro := Trim(edtEmitBairro.Text);
    Emit.enderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.enderEmit.xMun    := Trim(edtEmitCidade.Text);
    Emit.enderEmit.CEP     := StrToInt(edtEmitCEP.Text);
    Emit.enderEmit.UF      := Trim(edtEmitUF.Text);
    Emit.enderEmit.fone    := Trim(edtEmitFone.Text);

    //Adiciona dados do tomador do serviço
    toma.CNPJCPF           := '10242141000174';
    toma.IE                := '0010834420031';
    toma.xNome             := 'ACOUGUE E SUPERMERCADO SOUZA LTDA';
    toma.xFant             := '';
    toma.fone              := '';
    toma.enderToma.xLgr    := 'RUA BELO HORIZONTE';
    toma.enderToma.nro     := '614';
    toma.enderToma.xCpl    := 'N D';
    toma.enderToma.xBairro := 'CALADINA';
    toma.enderToma.cMun    := 3119401;
    toma.enderToma.xMun    := 'CORONEL FABRICIANO';
    toma.enderToma.CEP     := 35171167;
    toma.enderToma.UF      := 'MG';
    toma.enderToma.cPais   := 1058;
    toma.enderToma.xPais   := 'BRASIL';
    toma.email             := '';

    {Carrega valores da prestacao de servico}
    vPrest.vTPrest         := 100.00;
    vPrest.vRec            := 100.00;

    {Carrega componentes do valor da prestacao}
    with vPrest.comp.New do
    begin
      xNome                := 'DFRNER KRTJ';
      vComp                := 374347.00;
    end;

    {Carrega Impostos}
    //00 - Tributação Normal ICMS
    {Imp.ICMS.SituTrib    := cst00;
    Imp.ICMS.ICMS00.CST  := cst00;
    Imp.ICMS.ICMS00.vBC  := Impostos.Vbc;
    Imp.ICMS.ICMS00.pICMS:= Impostos.Picms;
    Imp.ICMS.ICMS00.vICMS:= Impostos.Vicms;

    //40 - ICMS Isento
    Imp.ICMS.SituTrib  := cst40;
    Imp.ICMS.ICMS45.CST:= cst40;

    //41 - ICMS não Tributada
    Imp.ICMS.SituTrib  := cst41;
    Imp.ICMS.ICMS45.CST:= cst41;

    //51 - ICMS diferido
    Imp.ICMS.SituTrib  := cst51;
    Imp.ICMS.ICMS45.CST:= cst51; }

    //90 - ICMS Outros
    if Emit.enderEmit.UF = Rem.enderReme.UF then
    begin
      Imp.ICMS.SituTrib     := cst90;
      Imp.ICMS.ICMS90.CST   := cst90;
      Imp.ICMS.ICMS90.pRedBC:= 10.00;
      Imp.ICMS.ICMS90.vBC   := 100.00;
      Imp.ICMS.ICMS90.pICMS := 7.00;
      Imp.ICMS.ICMS90.vICMS := 6.30;
      Imp.ICMS.ICMS90.vCred := 0.00;
    end
    else
    begin
      Imp.ICMS.SituTrib                  := cstICMSOutraUF;
      Imp.ICMS.ICMSOutraUF.CST           := cstICMSOutraUF; // ICMS Outros
      Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := 0;
      Imp.ICMS.ICMSOutraUF.vBCOutraUF    := 100.00;
      Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := 7.00;
      Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := 7.00;
    end;

    //SN - Simples Nacional
    {Imp.ICMS.SituTrib     := cstICMSSN;
    Imp.ICMS.ICMSSN.indSN := 1;}

    Imp.infAdFisco := 'Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preço deste servico é de R$ 17,00 (17,00%) Fonte: IBPT';
    imp.vTotTrib   := 17.00;

    //Impostos federais
    imp.infTribFed.vPIS    := 1;
    imp.infTribFed.vCOFINS := 2;
    imp.infTribFed.vIR     := 3;
    imp.infTribFed.vINSS   := 4.00;
    imp.infTribFed.vCSLL   := 5;

    {Carrega as informacoes CTe Normal}
    infCTeNorm.infServico.xDescServ := 'TEJEJRBEFR ERFERF TESTET JFREJ';
    infCTeNorm.infServico.qCarga    := 5000.0000;

    {Informações dos documentos referenciados}
    {with infCTeNorm.infDocRef.Add do
    begin
      nDoc     := '';
      serie    := '';
      subserie := '';
      dEmi     := Date;
      vDoc     := 0.00;
    end;}

    {Carrega informacoes do seguro}
    with infCTeNorm.Seg.New do
    begin
      respSeg := rsTomadorServico;
      xSeg    := 'TESTE';
      nApol   := '3743784738473847';
    end;

    {Carrega Informacoes do Modal}
    {Rodoviario}
    infCTeNorm.rodoOS.TAF            := '454545445454';
    infCTeNorm.rodoOS.NroRegEstadual := '';

    {Carega inf veiculos do modal rodo 0-1}
    with infCTeNorm.rodoOS.veic do
    begin
      placa    := 'MBC2448';
      RENAVAM  := '00709229895';
      UF       := 'SC';
      //Se for de Terceiro
      {Prop.CNPJCPF        := '';
      Prop.xNome          := '';
      Prop.UF             := '';
      prop.TAF            := '';
      prop.NroRegEstadual := '';
      Prop.IE             := '';
      Prop.tpProp         := tpTACAgregado;}
    end;

    {Carrega dados da CTe substituta 0-1}
    {with infCTeNorm.infCTeSub do
    begin
      chCte := '';
      //Se tomador não é Contribuinte
        tomaNaoICMS.refCteAnu := '';

      //Se tomador for Contribuinte
        case TipoDoc of //Tipo do Documento que o Tomador Emitiu para anulação de valor do Cte Anterior
          0: tomaICMS.refNFe := '';//NFe
          1: tomaICMS.refCte := '';//CTe
          2://NF
          begin
            tomaICMS.refNF.CNPJCPF  := '';
            tomaICMS.refNF.modelo   := '';
            tomaICMS.refNF.serie    := 0;
            tomaICMS.refNF.subserie := 0;
            tomaICMS.refNF.nro      := 0;
            tomaICMS.refNF.valor    := 0;
            tomaICMS.refNF.dEmi     := Date;
          end;
        end;
    end;}


    {CTe de Complemento de valor}
      //InfCTeComp.chave := '';

    {CTe de Anulacao de valores}
      //infCteAnu.chCTe := '';
      //infCteAnu.dEmi  := Date;

    {Seleciona o dados dos Autorizados a baixar o xml}
      //autXML.Add.CNPJCPF := '';
  end;
end;

procedure TfrmDemo_ACBrCTe.sbtnCaminhoCertClick(Sender: TObject);
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

procedure TfrmDemo_ACBrCTe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrCTe1.SSL.SelecionarCertificado;
end;

procedure TfrmDemo_ACBrCTe.sbtnLogoMarcaClick(Sender: TObject);
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

procedure TfrmDemo_ACBrCTe.sbtnPathSalvarClick(Sender: TObject);
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

procedure TfrmDemo_ACBrCTe.FormCreate(Sender: TObject);
var
 T: TSSLLib;
 U: TSSLCryptLib;
 V: TSSLHttpLib;
 X: TSSLXmlSignLib;
 Y: TSSLType;
begin
  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLLib), integer(T) ) ) ;
  cbSSLLib.ItemIndex := 0 ;

  cbCryptLib.Items.Clear ;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add( GetEnumName(TypeInfo(TSSLCryptLib), integer(U) ) ) ;
  cbCryptLib.ItemIndex := 0 ;

  cbHttpLib.Items.Clear ;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(V) ) ) ;
  cbHttpLib.ItemIndex := 0 ;

  cbXmlSignLib.Items.Clear ;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add( GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X) ) ) ;
  cbXmlSignLib.ItemIndex := 0 ;

  cbSSLType.Items.Clear ;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(Y) ) ) ;
  cbSSLType.ItemIndex := 0 ;

  LerConfiguracao;
end;

procedure TfrmDemo_ACBrCTe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  LerConfiguracao;
end;

procedure TfrmDemo_ACBrCTe.lblColaboradorClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/5', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrCTe.lblPatrocinadorClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/35', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrCTe.lblDoar1Click(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/14', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrCTe.ACBrCTe1StatusChange(Sender: TObject);
begin
  case ACBrCTe1.Status of
    stCTeIdle : begin
                  if ( frmStatus <> nil ) then
                    frmStatus.Hide;
                end;
    stCTeStatusServico : begin
                           if ( frmStatus = nil ) then
                             frmStatus := TfrmStatus.Create(Application);

                           frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
                           frmStatus.Show;
                           frmStatus.BringToFront;
                         end;
    stCTeRecepcao : begin
                      if ( frmStatus = nil ) then
                        frmStatus := TfrmStatus.Create(Application);

                      frmStatus.lblStatus.Caption := 'Enviando dados do CTe...';
                      frmStatus.Show;
                      frmStatus.BringToFront;
                    end;
    stCTeRetRecepcao : begin
                         if ( frmStatus = nil ) then
                           frmStatus := TfrmStatus.Create(Application);

                         frmStatus.lblStatus.Caption := 'Recebendo dados do CTe...';
                         frmStatus.Show;
                         frmStatus.BringToFront;
                       end;
    stCTeConsulta : begin
                      if ( frmStatus = nil ) then
                        frmStatus := TfrmStatus.Create(Application);

                      frmStatus.lblStatus.Caption := 'Consultando CTe...';
                      frmStatus.Show;
                      frmStatus.BringToFront;
                    end;
    stCTeCancelamento : begin
                          if ( frmStatus = nil ) then
                            frmStatus := TfrmStatus.Create(Application);

                          frmStatus.lblStatus.Caption := 'Enviando cancelamento de CTe...';
                          frmStatus.Show;
                          frmStatus.BringToFront;
                        end;
    stCTeInutilizacao : begin
                          if ( frmStatus = nil ) then
                            frmStatus := TfrmStatus.Create(Application);

                          frmStatus.lblStatus.Caption := 'Enviando pedido de Inutilização...';
                          frmStatus.Show;
                          frmStatus.BringToFront;
                        end;
    stCTeRecibo : begin
                    if ( frmStatus = nil ) then
                      frmStatus := TfrmStatus.Create(Application);

                    frmStatus.lblStatus.Caption := 'Consultando Recibo de Lote...';
                    frmStatus.Show;
                    frmStatus.BringToFront;
                  end;
    stCTeCadastro : begin
                      if ( frmStatus = nil ) then
                        frmStatus := TfrmStatus.Create(Application);

                      frmStatus.lblStatus.Caption := 'Consultando Cadastro...';
                      frmStatus.Show;
                      frmStatus.BringToFront;
                    end;
    stCTeEmail : begin
                   if ( frmStatus = nil ) then
                     frmStatus := TfrmStatus.Create(Application);

                   frmStatus.lblStatus.Caption := 'Enviando Email...';
                   frmStatus.Show;
                   frmStatus.BringToFront;
                 end;
  end;

  Application.ProcessMessages;
end;

procedure TfrmDemo_ACBrCTe.AtualizaSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer( ACBrCTe1.Configuracoes.Geral.SSLLib );
  cbCryptLib.ItemIndex   := Integer( ACBrCTe1.Configuracoes.Geral.SSLCryptLib );
  cbHttpLib.ItemIndex    := Integer( ACBrCTe1.Configuracoes.Geral.SSLHttpLib );
  cbXmlSignLib.ItemIndex := Integer( ACBrCTe1.Configuracoes.Geral.SSLXmlSignLib );

  cbSSLType.Enabled      := (ACBrCTe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]) ;
end;

procedure TfrmDemo_ACBrCTe.ACBrCTe1GerarLog(const Mensagem: String);
begin
  memoLog.Lines.Add(Mensagem);
end;

procedure TfrmDemo_ACBrCTe.btnStatusServClick(Sender: TObject);
begin
  with ACBrCTe1.WebServices do
  begin
    StatusServico.Executar;

    MemoResp.Lines.Text   := UTF8Encode(StatusServico.RetWS);
    memoRespWS.Lines.Text := UTF8Encode(StatusServico.RetWS);

    LoadXML(MemoResp, WBResposta);

    PageControl2.ActivePageIndex := 5;

    with MemoDados do
    begin
      Lines.Add('');
      Lines.Add('Status Serviço');
      Lines.Add('tpAmb: '    + TpAmbToStr(StatusServico.tpAmb));
      Lines.Add('verAplic: ' + StatusServico.verAplic);
      Lines.Add('cStat: '    + IntToStr(StatusServico.cStat));
      Lines.Add('xMotivo: '  + StatusServico.xMotivo);
      Lines.Add('cUF: '      + IntToStr(StatusServico.cUF));
      Lines.Add('dhRecbto: ' + DateTimeToStr(StatusServico.dhRecbto));
      Lines.Add('tMed: '     + IntToStr(StatusServico.TMed));
      Lines.Add('dhRetorno: '+ DateTimeToStr(StatusServico.dhRetorno));
      Lines.Add('xObs: '     + StatusServico.xObs);
    end;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnInutilizarClick(Sender: TObject);
var
  Serie, Ano, NumeroInicial, NumeroFinal, Justificativa: String;
begin
  if not(InputQuery('WebServices Inutilização ', 'Ano',    Ano)) then
    exit;

  if not(InputQuery('WebServices Inutilização ', 'Serie',  Serie)) then
    exit;

  if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroInicial)) then
    exit;

  if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroFinal)) then
    exit;

  if not(InputQuery('WebServices Inutilização ', 'Justificativa', Justificativa)) then
    exit;

  ACBrCTe1.Inutilizar(edtEmitCNPJ.Text, Justificativa, StrToInt(Ano), StrToInt(Serie),
                      StrToInt(NumeroInicial), StrToInt(NumeroFinal));

  MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Inutilizacao.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Inutilizacao.RetWS);

  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrCTe.btnGerarCTeClick(Sender: TObject);
var
  vAux: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Conhecimento', vAux)) then
    exit;

  ACBrCTe1.Conhecimentos.Clear;

  if rgModeloDF.ItemIndex = 0 then
    GerarCTe(vAux)
  else
    GerarCTeOS(vAux);

  ACBrCTe1.Conhecimentos.Items[0].GravarXML('','');
  ACBrCTe1.Conhecimentos.Assinar;

  ShowMessage('Arquivo gerado em: ' + ACBrCTe1.Conhecimentos.Items[0].NomeArq);

  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrCTe1.Conhecimentos.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrCTe1.Conhecimentos.Items[0].NomeArq);

  LoadXML(MemoResp, WBResposta);

  PageControl2.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrCTe.btnConsCadClick(Sender: TObject);
var
  UF, Documento: String;
begin
  if not(InputQuery('WebServices Consulta Cadastro ', 'UF do Documento a ser Consultado:', UF)) then
    exit;
  if not(InputQuery('WebServices Consulta Cadastro ', 'Documento(CPF/CNPJ)', Documento)) then
    exit;

  Documento := Trim(OnlyNumber(Documento));

  ACBrCTe1.WebServices.ConsultaCadastro.UF := UF;

  if Length(Documento) > 11 then
    ACBrCTe1.WebServices.ConsultaCadastro.CNPJ := Documento
  else
    ACBrCTe1.WebServices.ConsultaCadastro.CPF := Documento;

  ACBrCTe1.WebServices.ConsultaCadastro.Executar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.ConsultaCadastro.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.ConsultaCadastro.RetWS);

  LoadXML(MemoResp, WBResposta);

  ShowMessage(ACBrCTe1.WebServices.ConsultaCadastro.xMotivo);
  ShowMessage(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xNome);
end;

procedure TfrmDemo_ACBrCTe.btnCriarEnviarClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Conhecimento', vAux)) then
    exit;

  if not(InputQuery('WebServices Enviar', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrCTe1.Conhecimentos.Clear;

  if rgModeloDF.ItemIndex = 0 then
    GerarCTe(vAux)
  else
    GerarCTeOS(vAux);

  ACBrCTe1.Enviar(StrToInt(vNumLote));

  MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Retorno.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Retorno.RetWS);

  LoadXML(MemoResp, WBResposta);

  PageControl2.ActivePageIndex := 5;

  with MemoDados do
  begin
    Lines.Add('');
    Lines.Add('Envio CTe');
    Lines.Add('tpAmb: '     + TpAmbToStr(ACBrCTe1.WebServices.Retorno.tpAmb));
    Lines.Add('verAplic: '  + ACBrCTe1.WebServices.Retorno.verAplic);
    Lines.Add('cStat: '     + IntToStr(ACBrCTe1.WebServices.Retorno.cStat));
    Lines.Add('xMotivo: '   + ACBrCTe1.WebServices.Retorno.xMotivo);
    Lines.Add('cUF: '       + IntToStr(ACBrCTe1.WebServices.Retorno.cUF));
    Lines.Add('xMsg: '      + ACBrCTe1.WebServices.Retorno.Msg);
    Lines.Add('Recibo: '    + ACBrCTe1.WebServices.Retorno.Recibo);
    Lines.Add('Protocolo: ' + ACBrCTe1.WebServices.Retorno.Protocolo);
  end;
end;

procedure TfrmDemo_ACBrCTe.btnCriarEnviarSincronoClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  if not(InputQuery('WebServices Enviar Síncrono', 'Numero do Conhecimento', vAux)) then
    exit;

  if not(InputQuery('WebServices Enviar Síncrono', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrCTe1.Conhecimentos.Clear;

  if rgModeloDF.ItemIndex = 0 then
    GerarCTe(vAux)
  else
    GerarCTeOS(vAux);

  // Parâmetros do método Enviar:
  // 1o = Número do Lote
  // 2o = Se True imprime automaticamente o DACTE
  // 3o = Se True o envio é no modo Síncrono, caso contrario Assíncrono.
  // Obs: no modo Síncrono só podemos enviar UM CT-e por vez.
  ACBrCTe1.Enviar(StrToInt(vNumLote), True, True);

  MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Retorno.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Retorno.RetWS);

  LoadXML(MemoResp, WBResposta);

  PageControl2.ActivePageIndex := 5;

  with MemoDados do
  begin
    Lines.Add('');
    Lines.Add('Envio CTe');
    Lines.Add('tpAmb: '     + TpAmbToStr(ACBrCTe1.WebServices.Retorno.tpAmb));
    Lines.Add('verAplic: '  + ACBrCTe1.WebServices.Retorno.verAplic);
    Lines.Add('cStat: '     + IntToStr(ACBrCTe1.WebServices.Retorno.cStat));
    Lines.Add('xMotivo: '   + ACBrCTe1.WebServices.Retorno.xMotivo);
    Lines.Add('cUF: '       + IntToStr(ACBrCTe1.WebServices.Retorno.cUF));
    Lines.Add('xMsg: '      + ACBrCTe1.WebServices.Retorno.Msg);
    Lines.Add('Recibo: '    + ACBrCTe1.WebServices.Retorno.Recibo);
    Lines.Add('Protocolo: ' + ACBrCTe1.WebServices.Retorno.Protocolo);
  end;
end;

procedure TfrmDemo_ACBrCTe.btnConsultarReciboClick(Sender: TObject);
var
  aux: String;
begin
  if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux)) then
    exit;

  ACBrCTe1.WebServices.Recibo.Recibo := aux;
  ACBrCTe1.WebServices.Recibo.Executar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Recibo.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Recibo.RetWS);

  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrCTe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Consultar;

    ShowMessage(ACBrCTe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Consulta.RetWS);
    memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Consulta.RetWS);

    // Retorno do XML completo: CTe + Eventos vinculados
    MemoDados.Lines.Text :=  UTF8Encode(ACBrCTe1.WebServices.Consulta.RetCTeDFe);

    LoadXML(MemoResp, WBResposta);
  end;
end;

procedure TfrmDemo_ACBrCTe.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  if not(InputQuery('WebServices Consultar', 'Chave do CT-e:', vChave)) then
    exit;

  ACBrCTe1.WebServices.Consulta.CTeChave := vChave;
  ACBrCTe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Consulta.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Consulta.RetornoWS);

  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrCTe.btnEnvEPECClick(Sender: TObject);
//var
//  vAux: String;
begin
  ShowMessage('Opção não Implementada, no programa exemplo!');
end;

procedure TfrmDemo_ACBrCTe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg : String;
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.XML)|*-cte.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    PageControl2.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrCTe1.Conhecimentos.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrCTe1.SSL.CarregarCertificadoPublico( ACBrCTe1.Conhecimentos.Items[0].CTe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrCTe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrCTe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrCTe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnValidarXMLClick(Sender: TObject);
var
  Erros: string;
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Conhecimentos.Validar;
    ACBrCTe1.Conhecimentos.ValidarRegrasdeNegocios(Erros);

    showmessage('Conhecimento de Transporte Eletrônico Valido');
    showmessage(Erros);
  end;
end;

procedure TfrmDemo_ACBrCTe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCTe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCTe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCTe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    ACBrCTe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmDemo_ACBrCTe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnCancCTeClick(Sender: TObject);
var
  vAux: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe a ser Cancelado';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('Cancelamento do CTe:', 'Justificativa', vAux)) then
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      infEvento.nSeqEvento      := 1; // Para o Evento de Cancelamento: nSeqEvento sempre = 1
      infEvento.chCTe           := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ            := edtEmitCNPJ.Text;
      infEvento.dhEvento        := now;
      infEvento.tpEvento        := teCancelamento;
      infEvento.detEvento.xJust := trim(vAux);
      infEvento.detEvento.nProt := ACBrCTe1.Conhecimentos.Items[0].CTe.procCTe.nProt;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.EnvEvento.RetWS);
    memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.EnvEvento.RetWS);

    LoadXML(MemoResp, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmDemo_ACBrCTe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.DACTE.Cancelada := True;
    ACBrCTe1.Conhecimentos.Imprimir;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnGerarPDFClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
//    ACBrCTe1.Conhecimentos.ImprimirPDF;
  end;

  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
//    ACBrCTe1.Conhecimentos.GerarPDF;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    CC:=TstringList.Create;
    CC.Add('email_1@provedor.com'); //especifique um email válido
    CC.Add('email_2@provedor.com.br'); //especifique um email válido

    ACBrCTe1.Conhecimentos.Items[0].EnviarEmail(Para
                                              , edtEmailAssunto.Text
                                              , mmEmailMsg.Lines
                                              , False //Enviar PDF junto
                                              , nil //Lista com emails que serão enviado cópias - TStrings
                                              , nil // Lista de anexos - TStrings
                                               );
    CC.Free;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  ACBrCTe1.Conhecimentos.Clear;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*-procEventoCTe.xml';
  OpenDialog1.Filter := 'Arquivos Evento (*-procEventoCTe.xml)|*-procEventoCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirEvento;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnGerarPDFEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  ACBrCTe1.Conhecimentos.Clear;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*-procEventoCTe.xml';
  OpenDialog1.Filter := 'Arquivos Evento (*-procEventoCTe.xml)|*-procEventoCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirEventoPDF;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*-procEventoCTe.xml';
  OpenDialog1.Filter := 'Arquivos Evento (*-procEventoCTe.xml)|*-procEventoCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    Evento := TStringList.Create;
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);

    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.LerXML(OpenDialog1.FileName);

    CC:=TstringList.Create;
    CC.Add('andrefmoraes@gmail.com'); //especifique um email válido
    CC.Add('anfm@zipmail.com.br');    //especifique um email válido

    ACBrCTe1.EnviarEmailEvento(Para, edtEmailAssunto.Text, mmEmailMsg.Lines,
                               nil, //Lista com emails que serão enviado cópias - TStrings
                               nil, // Lista de anexos - TStrings
                               nil  // ReplyTo
                               );

    CC.Free;
    Evento.Free;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnImprimirInutClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o ProcInutCTe';
  OpenDialog1.DefaultExt := '*-ProcInutCTe.xml';
  OpenDialog1.Filter := 'Arquivos ProcInutCTe (*-ProcInutCTe.xml)|*-ProcInutCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.InutCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirInutilizacao;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnGerarPDFInutClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o ProcInutCTe';
  OpenDialog1.DefaultExt := '*-ProcInutCTe.xml';
  OpenDialog1.Filter := 'Arquivos ProcInutCTe (*-ProcInutCTe.xml)|*-ProcInutCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.InutCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirInutilizacaoPDF;
  end;
end;

end.
