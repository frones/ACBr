{$I ACBr.inc}

unit Frm_Demo_ACBrCTe;

interface

uses IniFiles,
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,  StdCtrls, Buttons, ExtCtrls, IpHtml, pcteConversaoCTe, ACBrUtil,
  pcnConversao, ACBrCTe, ACBrCTeDACTEClass, ACBrCTeDACTeRLClass;

type

  { TfrmDemo_ACBrCTe }

  TfrmDemo_ACBrCTe = class(TForm)
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    btnGerarPDF1: TButton;
    WBResposta: TIpHtmlPanel;
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
    btnImportarXML: TButton;
    btnConsultarChave: TButton;
    btnCancelarChave: TButton;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
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
    procedure btnGerarPDF1Click(Sender: TObject);
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
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnInutilizarClick(Sender: TObject);
    procedure btnConsultarReciboClick(Sender: TObject);
    procedure btnConsCadClick(Sender: TObject);
    procedure btnGerarCTeClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnEnvEPECClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnImportarXMLClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure ACBrCTe1StatusChange(Sender: TObject);
    procedure ACBrCTe1GerarLog(const Mensagem: String);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
    procedure btnGerarPDFEventoClick(Sender: TObject);
    {
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    }
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TIpHtmlPanel);
    procedure GerarCTe(NumCTe : String);
  public
    { Public declarations }
  end;

var
  frmDemo_ACBrCTe: TfrmDemo_ACBrCTe;

implementation

uses
 FileCtrl, DateUtils,
 ufrmStatus,
 {ACBrCTeConhecimentos,} ACBrDFeUtil{, ACBrCTeUtil};

const
  SELDIRHELP = 1000;

{$R *.lfm}

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
 IniFile    : String;
 Ini        : TIniFile;
 StreamMemo : TMemoryStream;
begin
 IniFile := ChangeFileExt( Application.ExeName, '.ini');

 Ini := TIniFile.Create( IniFile );
 try
  Ini.WriteString( 'Certificado','Caminho' ,edtCaminho.Text);
  Ini.WriteString( 'Certificado','Senha'   ,edtSenha.Text);
  Ini.WriteString( 'Certificado','NumSerie',edtNumSerie.Text);

  Ini.WriteInteger( 'Geral','DACTE'       ,rgTipoDACTe.ItemIndex);
  Ini.WriteInteger( 'Geral','FormaEmissao',rgFormaEmissao.ItemIndex);
  Ini.WriteString( 'Geral','LogoMarca'   ,edtLogoMarca.Text);
  Ini.WriteBool(   'Geral','Salvar'      ,ckSalvar.Checked);
  Ini.WriteString( 'Geral','PathSalvar'  ,edtPathLogs.Text);

  Ini.WriteString( 'WebService','UF'        ,cbUF.Text);
  Ini.WriteInteger( 'WebService','Ambiente'  ,rgTipoAmb.ItemIndex);
  Ini.WriteBool(   'WebService','Visualizar',ckVisualizar.Checked);

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
 IniFile    : String;
 Ini        : TIniFile;
 Ok         : Boolean;
 StreamMemo : TMemoryStream;
begin
 IniFile := ChangeFileExt( Application.ExeName, '.ini');

 Ini := TIniFile.Create( IniFile );
 try
  {$IFDEF ACBrCTeOpenSSL}
   edtCaminho.Text  := Ini.ReadString( 'Certificado','Caminho' ,'');
   edtSenha.Text    := Ini.ReadString( 'Certificado','Senha'   ,'');
   ACBrCTe1.Configuracoes.Certificados.Certificado  := edtCaminho.Text;
   ACBrCTe1.Configuracoes.Certificados.Senha        := edtSenha.Text;
   edtNumSerie.Visible := False;
   Label25.Visible     := False;
   sbtnGetCert.Visible := False;
  {$ELSE}
   edtNumSerie.Text := Ini.ReadString( 'Certificado','NumSerie','');
   ACBrCTe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;
   //edtNumSerie.Text := ACBrCTe1.Configuracoes.Certificados.NumeroSerie;
   Label1.Caption := 'Informe o número de série do certificado'#13+
                     'Disponível no Internet Explorer no menu'#13+
                     'Ferramentas - Opções da Internet - Conteúdo '#13+
                     'Certificados - Exibir - Detalhes - '#13+
                     'Número do certificado';
   Label2.Visible     := False;
   edtCaminho.Visible := False;
   edtSenha.Visible   := False;
   sbtnCaminhoCert.Visible := False;
  {$ENDIF}

  rgFormaEmissao.ItemIndex := Ini.ReadInteger('Geral','FormaEmissao',0);
  ckSalvar.Checked         := Ini.ReadBool(   'Geral','Salvar'      ,True);
  edtPathLogs.Text         := Ini.ReadString( 'Geral','PathSalvar'  ,'');

  case rgFormaEmissao.ItemIndex of
   0: ACBrCTe1.Configuracoes.Geral.FormaEmissao := teNormal;
   1: ACBrCTe1.Configuracoes.Geral.FormaEmissao := teDPEC; // o mesmo que EPEC
   2: ACBrCTe1.Configuracoes.Geral.FormaEmissao := teFSDA;
   3: ACBrCTe1.Configuracoes.Geral.FormaEmissao := teSVCRS;
   4: ACBrCTe1.Configuracoes.Geral.FormaEmissao := tESVCSP;
  end;
  
  ACBrCTe1.Configuracoes.Geral.Salvar       := ckSalvar.Checked;
  ACBrCTe1.Configuracoes.Arquivos.PathSalvar   := edtPathLogs.Text;

  cbUF.ItemIndex       := cbUF.Items.IndexOf(Ini.ReadString('WebService','UF','SP'));
  rgTipoAmb.ItemIndex  := Ini.ReadInteger('WebService','Ambiente'  ,0);
  ckVisualizar.Checked :=Ini.ReadBool(    'WebService','Visualizar',False);
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

  rgTipoDACTe.ItemIndex := Ini.ReadInteger( 'Geral','DACTE'       ,0);
  edtLogoMarca.Text     := Ini.ReadString( 'Geral','LogoMarca'   ,'');
  if ACBrCTe1.DACTe <> nil then
   begin
    ACBrCTe1.DACTe.TipoDACTe := StrToTpImp(OK,IntToStr(rgTipoDaCTe.ItemIndex+1));
    ACBrCTe1.DACTe.Logo      := edtLogoMarca.Text;
    ACBrCTe1.DACTe.PathPDF   := edtPathLogs.Text;
    ACBrCTe1.DACTe.TamanhoPapel := tpA4;
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
 finally
  Ini.Free;
 end;
end;

procedure TfrmDemo_ACBrCTe.LoadXML(MyMemo: TMemo;  MyWebBrowser: TIpHtmlPanel);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
     MyMemo.Lines.SaveToStream(ms) ;
     ms.Seek(0, 0);
     MyWebBrowser.SetHtmlFromStream(ms);
  finally
     ms.Free;
  end;
 //MyMemo.Lines.SaveToFile(ExtractFileDir(application.ExeName)+'temp.xml');
 //MyWebBrowser.Navigate(ExtractFileDir(application.ExeName)+'temp.xml');
end;

procedure TfrmDemo_ACBrCTe.GerarCTe(NumCTe: String);
var
 i, j, CodigoMunicipio, Tomador: Integer;
begin

 // O código abaixo faz parte da minha aplicação devendo ser feitas as alterações
 // necessárias para ser utilizado na sua.
 
 (*
 with DMCTE.CTe.Conhecimentos.Add.CTe do
  begin
   //
   // Dados de Identificação do CT-e
   //
   Ide.cUF:=DM_CTA.EmpresaCodigoEstado.AsInteger;
   Ide.cCT:=DM_CNT.Conhec2CTChave.AsInteger;  // Código Aleatório
   Ide.CFOP:=DM_CNT.Conhec2CFOP.AsInteger;
   Ide.natOp:='PRESTAÇÃO DE SERVIÇO';
   if DM_CNT.Conhec2ForPag.AsInteger=0
    then Ide.forPag:=fpPago
    else Ide.forPag:=fpAPagar;
   Ide.modelo:='57';
   Ide.serie:=DM_CNT.Conhec2Serie.AsInteger;
   Ide.nCT:=DM_CNT.Conhec2Numero.AsInteger;
   Ide.dhEmi:=Now;
   Ide.tpImp:=tiRetrato;

   // TpcnTipoEmissao = (teNormal, teContingencia, teSCAN, teDPEC, teFSDA);
   case DM_CNT.ParametrosCTeGeralFormaEm.AsInteger of
    0: Ide.tpEmis:=teNormal;
    1: Ide.tpEmis:=teContingencia;
    2: Ide.tpEmis:=teSCAN;
    3: Ide.tpEmis:=teDPEC;
    4: Ide.tpEmis:=teFSDA;
   end;

   // TpcnTipoAmbiente = (taProducao, taHomologacao);
   case DM_CNT.ParametrosCTeWebServAmbiente.AsInteger of
    0: Ide.tpAmb:=taHomologacao;
    1: Ide.tpAmb:=taProducao;
   end;

   // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
   case DM_CNT.Conhec2TipoCTe.AsInteger of
    0: Ide.tpCTe:=tcNormal;
    1: Ide.tpCTe:=tcComplemento;
    2: Ide.tpCTe:=tcAnulacao;
    3: Ide.tpCTe:=tcSubstituto;
   end;

   // TpcnProcessoEmissao = (peAplicativoContribuinte, peAvulsaFisco, peAvulsaContribuinte, peContribuinteAplicativoFisco);
   Ide.procEmi:=peAplicativoContribuinte;
   Ide.verProc:='4.0';
   // Ide.refCTE:=''; // Chave de Acesso do CT-e Referenciado
   CodigoMunicipio:=DM_CTA.EmpresaCodigoEstado.AsInteger * 100000 +
                    DM_CTA.EmpresaCodigoMunicipio.AsInteger;
   Ide.cMunEmi:=CodigoMunicipio;
   Ide.xMunEmi:=DM_CTA.EmpresaCidade.AsString;
   Ide.UFEmi:=DM_CTA.EmpresaEstado.AsString;

   // TpcteModal = (mdRodoviario, mdAereo, mdAquaviario, mdFerroviario, mdDutoviario);
   Ide.modal:=mdRodoviario;

   // TpcteTipoServico = (tsNormal, tsSubcontratacao, tsRedespacho, tsIntermediario);
   case DM_CNT.Conhec2TipoServico.AsInteger of
    0: Ide.tpServ:=tsNormal;
    1: Ide.tpServ:=tsSubcontratacao;
    2: Ide.tpServ:=tsRedespacho;
    3: Ide.tpServ:=tsIntermediario;
   end;

   // Origem da Prestação
   Ide.cMunIni:=DM_CNT.Conhec2CodCidadeColeta.AsInteger;
   Ide.xMunIni:=DM_CNT.Conhec2NomeCidadeColeta.AsString;
   Ide.UFIni:=DM_CNT.Conhec2EstadoColeta.AsString;

   // Destino da Prestação
   Ide.cMunFim:=DM_CNT.Conhec2CodCidadeEntrega.AsInteger;
   Ide.xMunFim:=DM_CNT.Conhec2NomeCidadeEntrega.AsString;
   Ide.UFFim:=DM_CNT.Conhec2EstadoEntrega.AsString;

   // TpcteRetira = (rtSim, rtNao);
   Ide.retira:=rtSim;
   Ide.xdetretira:='';

   case DM_CNT.Conhec2TomadorServico.AsInteger of
    0: Ide.Toma03.Toma:=tmRemetente;
    1: Ide.Toma03.Toma:=tmExpedidor;
    2: Ide.Toma03.Toma:=tmRecebedor;
    3: Ide.Toma03.Toma:=tmDestinatario;
    4: Ide.Toma03.Toma:=tmRemetente;
   end;

   // Totamdor do Serviço no CTe 4 = Outros
   if DM_CNT.Conhec2TomadorServico.AsInteger=4
    then begin
     DM_CTA.PessoaFJ.Close;
     DM_CTA.PessoaFJ.SQL.Clear;
     DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
     DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
     DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.Conhec2Outros.AsString;
     DM_CTA.PessoaFJ.Active:=True;
     DM_CTA.PessoaFJ.Open;

     Ide.Toma4.Toma:=tmOutros;
     if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
          then begin
           Ide.Toma4.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14);
           IE := DM_CTA.PessoaFJIEstadual.AsString;
          end
          else begin
           Ide.Toma4.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                                   Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);
           IE := 'ISENTO';
          end;
     Ide.Toma4.IE:=IE;
     Ide.Toma4.xNome:=DM_CTA.PessoaFJRSocial.AsString;
     Ide.Toma4.xFant:=DM_CTA.PessoaFJFantasia.AsString;
     Ide.Toma4.fone:=DM_CTA.PessoaFJTelefone.AsString;
     Ide.Toma4.EnderToma.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
     Ide.Toma4.EnderToma.xNum:=DM_CTA.PessoaFJNumero.AsString;
     Ide.Toma4.EnderToma.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
     Ide.Toma4.EnderToma.xBairro:=DM_CTA.PessoaFJBairro.AsString;

     CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
     Ide.Toma4.EnderToma.cMun:=CodigoMunicipio;
     Ide.Toma4.EnderToma.xMun:=DM_CTA.PessoaFJCidade.AsString;
     Ide.Toma4.EnderToma.CEP:=StrToIntDef(DM_CTA.PessoaFJCEP.AsString, 0);
     Ide.Toma4.EnderToma.UF:=DM_CTA.PessoaFJEstado.AsString;
     Ide.Toma4.EnderToma.cPais:=DM_CTA.PessoaFJCodigoPais.AsInteger;
     Ide.Toma4.EnderToma.xPais:=DM_CTA.PessoaFJPais.AsString;
    end;

   //
   //  Informações Complementares do CTe
   //
   compl.xCaracAd := Trim(DM_CNT.Conhec2CaracAdTrans.AsString);
   compl.xCaracSer := Trim(DM_CNT.Conhec2CaracAdServ.AsString);
   compl.xEmi := Trim(DM_CNT.Conhec2FuncioEmissorCTe.AsString);

   compl.fluxo.xOrig := Trim(DM_CNT.Conhec2FluxoOrigem.AsString);
   if Trim(DM_CNT.Conhec2FluxoPassagem.AsString)<>''
    then begin
     with compl.fluxo.pass.Add do
      begin
       xPass := Trim(DM_CNT.Conhec2FluxoPassagem.AsString);
      end;
    end;
   compl.fluxo.xDest := Trim(DM_CNT.Conhec2FluxoDestino.AsString);
   compl.fluxo.xRota := Trim(DM_CNT.Conhec2FluxoRota.AsString);

   compl.Entrega.TipoData:=StrToTpDataPeriodo(okConversao, IntToStr(DM_CNT.Conhec2EntregaTipoData.AsInteger));
   case DM_CNT.Conhec2EntregaTipoData.AsInteger of
        0: compl.Entrega.semData.tpPer:=tdSemData;
    1,2,3: begin
            compl.Entrega.comData.tpPer:=StrToTpDataPeriodo(okConversao, IntToStr(DM_CNT.Conhec2EntregaTipoData.AsInteger));
            compl.Entrega.comData.dProg:=DM_CNT.Conhec2EntregaDataI.AsDateTime;
           end;
        4: begin
            compl.Entrega.noPeriodo.tpPer:=tdNoPeriodo;
            compl.Entrega.noPeriodo.dIni:=DM_CNT.Conhec2EntregaDataI.AsDateTime;
            compl.Entrega.noPeriodo.dFim:=DM_CNT.Conhec2EntregaDataF.AsDateTime;
           end;
   end;

   compl.Entrega.TipoHora:=StrToTpHorarioIntervalo(okConversao, IntToStr(DM_CNT.Conhec2EntregaTipoHora.AsInteger));
   case DM_CNT.Conhec2EntregaTipoHora.AsInteger of
        0: compl.Entrega.semHora.tpHor:=thSemHorario;
    1,2,3: begin
            compl.Entrega.comHora.tpHor:=StrToTpHorarioIntervalo(okConversao, IntToStr(DM_CNT.Conhec2EntregaTipoHora.AsInteger));
            compl.Entrega.comHora.hProg:=StrToTime(DM_CNT.Conhec2EntregaHoraI.AsString);
           end;
        4: begin
            compl.Entrega.noInter.tpHor:=thNoIntervalo;
            compl.Entrega.noInter.hIni:=StrToTime(DM_CNT.Conhec2EntregaHoraI.AsString);
            compl.Entrega.noInter.hFim:=StrToTime(DM_CNT.Conhec2EntregaHoraF.AsString);
           end;
   end;

   compl.origCalc := DM_CNT.Conhec2NomeCidadeColeta.AsString;
   compl.destCalc := DM_CNT.Conhec2NomeCidadeEntrega.AsString;
   compl.xObs     := DM_CNT.Conhec2Mensagem.AsString;

   DM_CNT.Tabelas.Close;
   DM_CNT.Tabelas.SQL.Clear;
   DM_CNT.Tabelas.SQL.Add('Select * From Cnt_Tabelas');
   DM_CNT.Tabelas.SQL.Add('Where Codigo = :xTabela');
   DM_CNT.Tabelas.Params[0].AsString:=DM_CNT.ParametrosTabela.AsString;
   DM_CNT.Tabelas.Active:=True;
   DM_CNT.Tabelas.Open;

   if DM_CNT.TabelasCST.AsInteger=41
    then compl.xObs := compl.xObs +
           ';Documento emitido por ME ou EPP optante pelo Simples Nacional' +
           ';Nao gera direito a credito fiscal de ICMS';

   // Obs do Contribuinte
   if (trim(DM_CNT.Conhec2ObsContCampo.AsString)<>'') and
      (trim(DM_CNT.Conhec2ObsContTexto.AsString)<>'')
    then begin
     with compl.ObsCont.Add do
      begin
       xCampo:=DM_CNT.Conhec2ObsContCampo.AsString;
       xTexto:=DM_CNT.Conhec2ObsContTexto.AsString;
      end;
    end;

   // Obs para o Fisco
   if (trim(DM_CNT.Conhec2ObsFiscoCampo.AsString)<>'') and
      (trim(DM_CNT.Conhec2ObsFiscoTexto.AsString)<>'')
    then begin
     with compl.ObsFisco.Add do
      begin
       xCampo:=DM_CNT.Conhec2ObsFiscoCampo.AsString;
       xTexto:=DM_CNT.Conhec2ObsFiscoTexto.AsString;
      end;
    end;

   //
   // Dados do Emitente
   //
   DM_CTA.Empresa.Close;
   DM_CTA.Empresa.SQL.Clear;
   DM_CTA.Empresa.SQL.Add('Select * From Sis_Empresa');
   DM_CTA.Empresa.SQL.Add('Where Codigo = :xCodigo');
   DM_CTA.Empresa.Params[0].AsString:=DM_CNT.ParametrosEmitente.AsString;
   DM_CTA.Empresa.Active:=True;
   DM_CTA.Empresa.Open;

   if copy(DM_CTA.EmpresaCNPJ.AsString,10,4)<>'0000'
    then Emit.CNPJ := Copy(DM_CTA.EmpresaCNPJ.AsString, 2, 14)
    else Emit.CNPJ := Copy(DM_CTA.EmpresaCNPJ.AsString, 1, 9) +
                      Copy(DM_CTA.EmpresaCNPJ.AsString, 14, 2);

   if (trim(DM_CTA.EmpresaInscEstadual.AsString)='') or
      (trim(DM_CTA.EmpresaInscEstadual.AsString)='ISENTO')
    then Emit.IE:='1234567890'
    else Emit.IE:=trim(DM_CTA.EmpresaInscEstadual.AsString);

   Emit.xNome:=DM_CTA.EmpresaNome.AsString;
   Emit.xFant:=DM_CTA.EmpresaFantasia.AsString;
   Emit.EnderEmit.xLgr:=DM_CTA.EmpresaEndereco.AsString;
   Emit.EnderEmit.nro:=DM_CTA.EmpresaNumero.AsString;
   Emit.EnderEmit.xCpl:=DM_CTA.EmpresaComplemento.AsString;
   Emit.EnderEmit.xBairro:=DM_CTA.EmpresaBairro.AsString;

   CodigoMunicipio:=DM_CTA.EmpresaCodigoEstado.AsInteger * 100000 +
                    DM_CTA.EmpresaCodigoMunicipio.AsInteger;
   Emit.EnderEmit.cMun:=CodigoMunicipio;
   Emit.EnderEmit.xMun:=DM_CTA.EmpresaCidade.AsString;
   Emit.EnderEmit.CEP:=StrToIntDef(DM_CTA.EmpresaCEP.AsString, 0);
   Emit.EnderEmit.UF:=DM_CTA.EmpresaEstado.AsString;
   Emit.EnderEmit.cPais:=DM_CTA.EmpresaCodigoPais.AsInteger;
   Emit.EnderEmit.xPais:=DM_CTA.EmpresaPais.AsString;
   Emit.EnderEmit.fone:=DM_CTA.EmpresaTelefone.AsString;

   //
   //  Dados do Remetente
   //
   if trim(DM_CNT.Conhec2Remetente.AsString)<>''
    then begin
     DM_CTA.PessoaFJ.Close;
     DM_CTA.PessoaFJ.SQL.Clear;
     DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
     DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
     DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.Conhec2Remetente.AsString;
     DM_CTA.PessoaFJ.Active:=True;
     DM_CTA.PessoaFJ.Open;

     Rem.xNome:=DM_CTA.PessoaFJRSocial.AsString;
     Rem.xFant:=DM_CTA.PessoaFJFantasia.AsString;
     Rem.EnderReme.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
     Rem.EnderReme.nro:=DM_CTA.PessoaFJNumero.AsString;
     Rem.EnderReme.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
     Rem.EnderReme.xBairro:=DM_CTA.PessoaFJBairro.AsString;

     CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
     Rem.EnderReme.cMun:=CodigoMunicipio;
     Rem.EnderReme.xMun:=DM_CTA.PessoaFJCidade.AsString;
     Rem.EnderReme.CEP:=StrToIntDef(DM_CTA.PessoaFJCEP.AsString, 0);
     Rem.EnderReme.UF:=DM_CTA.PessoaFJEstado.AsString;
     Rem.EnderReme.cPais:=DM_CTA.PessoaFJCodigoPais.AsInteger;
     Rem.EnderReme.xPais:=DM_CTA.PessoaFJPais.AsString;

     if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
      then begin
       Rem.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14);
       IE := DM_CTA.PessoaFJIEstadual.AsString;
      end
      else begin
       Rem.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                          Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);
       IE := 'ISENTO';
      end;

     Rem.IE:=IE;
     Rem.fone:=DM_CTA.PessoaFJTelefone.AsString;

     //
     // Relação das Notas Fiscais
     //
     DM_CNT.Notas.Close;
     DM_CNT.Notas.SQL.Clear;
     DM_CNT.Notas.SQL.Add('Select * From Cnt_Notas');
     DM_CNT.Notas.SQL.Add('Where Codigo = :xCodigo');
     DM_CNT.Notas.SQL.Add('and Numero = :xNumero');
     DM_CNT.Notas.SQL.Add('Order By Tipo');
     DM_CNT.Notas.Params[0].AsInteger:=DM_CNT.Conhec2Codigo.AsInteger;
     DM_CNT.Notas.Params[1].AsInteger:=DM_CNT.Conhec2Numero.AsInteger;
     DM_CNT.Notas.Active:=True;
     DM_CNT.Notas.Open;
     j:=DM_CNT.Notas.RecordCount;
     if j>0
      then begin
       DM_CNT.Notas.First;
       for i:=1 to j do
        begin
         case DM_CNT.NotasTipo.AsInteger of
          0: begin
              // Nota Fiscal
              with Rem.InfNF.Add do
               begin
                nRoma := DM_CNT.NotasRomaneioNF.AsString;
                nPed  := DM_CNT.NotasPedidoNF.AsString;
                serie := DM_CNT.NotasSerieNF.AsString;
                nDoc  := DM_CNT.NotasNumeroNF.AsString;
                dEmi  := DM_CNT.NotasEmissaoNF.AsDateTime;
                vBC   := RoundTo(DM_CNT.NotasValorBCICMS.AsFloat, -2);
                vICMS := RoundTo(DM_CNT.NotasValorICMS.AsFloat, -2);
                vBCST := RoundTo(DM_CNT.NotasValorBCICMSST.AsFloat, -2);
                vST   := RoundTo(DM_CNT.NotasValorICMSST.AsFloat, -2);
                vProd := RoundTo(DM_CNT.NotasValorProdutos.AsFloat, -2);
                vNF   := RoundTo(DM_CNT.NotasValorNF.AsFloat, -2);
                nCFOP := DM_CNT.NotasCFOPNF.AsInteger;
                nPeso := RoundTo(DM_CNT.NotasPesoKg.AsFloat, -2);
                PIN   := DM_CNT.NotasPinSuframa.AsString;

                // Local de Retirada

                if trim(DM_CNT.NotasCNPJRet.AsString)<>''
                 then begin
                  DM_CTA.PessoaFJ.Close;
                  DM_CTA.PessoaFJ.SQL.Clear;
                  DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
                  DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
                  DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.NotasCNPJRet.AsString;
                  DM_CTA.PessoaFJ.Active:=True;
                  DM_CTA.PessoaFJ.Open;

                  if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
                   then locRet.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14)
                   else locRet.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                          Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);

                  locRet.xNome:=DM_CTA.PessoaFJRSocial.AsString;
                  locRet.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
                  locRet.nro:=DM_CTA.PessoaFJNumero.AsString;
                  locRet.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
                  locRet.xBairro:=DM_CTA.PessoaFJBairro.AsString;
                  CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
                  locRet.cMun:=CodigoMunicipio;
                  locRet.xMun:=DM_CTA.PessoaFJCidade.AsString;
                  locRet.UF:=DM_CTA.PessoaFJEstado.AsString;
                 end;
               end;
             end;
          1: begin
              // Nota Fiscal Eletrônica
              with Rem.InfNFe.Add do
               begin
                chave := DM_CNT.NotasChaveNFe.AsString;
                PIN   := DM_CNT.NotasPinSuframa.AsString;
               end;
             end;
          2: begin
              // Outros Tipos de Documentos
              with Rem.InfOutros.Add do
               begin
                if DM_CNT.NotasTipoOutros.AsInteger = 0
                 then tpDoc := tdDeclaracao
                 else begin
                  tpDoc      := tdOutros;
                  descOutros := DM_CNT.NotasDescricao.AsString;
                 end;
                nDoc     := DM_CNT.NotasNumeroNF.AsString;
                dEmi     := DM_CNT.NotasEmissaoNF.AsDateTime;
                vDocFisc := RoundTo(DM_CNT.NotasValorNF.AsFloat, -2);
               end;
             end;
         end;
         DM_CNT.Notas.Next;
        end;
      end;
    end;

   //
   //  Dados do Destinatario
   //
   if trim(DM_CNT.Conhec2Destinatario.AsString)<>''
    then begin
     DM_CTA.PessoaFJ.Close;
     DM_CTA.PessoaFJ.SQL.Clear;
     DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
     DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
     DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.Conhec2Destinatario.AsString;
     DM_CTA.PessoaFJ.Active:=True;
     DM_CTA.PessoaFJ.Open;

     Dest.xNome:=DM_CTA.PessoaFJRSocial.AsString;
     Dest.EnderDest.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
     Dest.EnderDest.nro:=DM_CTA.PessoaFJNumero.AsString;
     Dest.EnderDest.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
     Dest.EnderDest.xBairro:=DM_CTA.PessoaFJBairro.AsString;

     CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
     Dest.EnderDest.cMun:=CodigoMunicipio;
     Dest.EnderDest.xMun:=DM_CTA.PessoaFJCidade.AsString;
     Dest.EnderDest.CEP:=StrToIntDef(DM_CTA.PessoaFJCEP.AsString, 0);
     Dest.EnderDest.UF:=DM_CTA.PessoaFJEstado.AsString;
     Dest.EnderDest.cPais:=DM_CTA.PessoaFJCodigoPais.AsInteger;
     Dest.EnderDest.xPais:=DM_CTA.PessoaFJPais.AsString;

     if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
      then begin
       Dest.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14);
       IE := DM_CTA.PessoaFJIEstadual.AsString;
      end
      else begin
       Dest.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                          Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);
       IE := 'ISENTO';
      end;

     Dest.IE:=IE;

     Dest.fone:=DM_CTA.PessoaFJTelefone.AsString;
     Dest.ISUF:=Trim(DM_CTA.PessoaFJInscSUF.AsString);

     // Local de Entrega
     if trim(DM_CNT.Conhec2CNPJEnt.AsString)<>''
      then begin
       DM_CTA.PessoaFJ.Close;
       DM_CTA.PessoaFJ.SQL.Clear;
       DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
       DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
       DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.Conhec2CNPJEnt.AsString;
       DM_CTA.PessoaFJ.Active:=True;
       DM_CTA.PessoaFJ.Open;

       if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
        then Dest.locEnt.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14)
        else Dest.locEnt.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                          Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);

       Dest.locEnt.xNome:=DM_CTA.PessoaFJRSocial.AsString;
       Dest.locEnt.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
       Dest.locEnt.nro:=DM_CTA.PessoaFJNumero.AsString;
       Dest.locEnt.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
       Dest.locEnt.xBairro:=DM_CTA.PessoaFJBairro.AsString;
       CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
       Dest.locEnt.cMun:=CodigoMunicipio;
       Dest.locEnt.xMun:=DM_CTA.PessoaFJCidade.AsString;
       Dest.locEnt.UF:=DM_CTA.PessoaFJEstado.AsString;
      end;
    end;

   //
   //  Dados do Expedidor
   //
   if trim(DM_CNT.Conhec2Expedidor.AsString)<>''
    then begin
     DM_CTA.PessoaFJ.Close;
     DM_CTA.PessoaFJ.SQL.Clear;
     DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
     DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
     DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.Conhec2Expedidor.AsString;
     DM_CTA.PessoaFJ.Active:=True;
     DM_CTA.PessoaFJ.Open;

     Exped.xNome:=DM_CTA.PessoaFJRSocial.AsString;
     Exped.EnderExped.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
     Exped.EnderExped.nro:=DM_CTA.PessoaFJNumero.AsString;
     Exped.EnderExped.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
     Exped.EnderExped.xBairro:=DM_CTA.PessoaFJBairro.AsString;

     CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
     Exped.EnderExped.cMun:=CodigoMunicipio;
     Exped.EnderExped.xMun:=DM_CTA.PessoaFJCidade.AsString;
     Exped.EnderExped.CEP:=StrToIntDef(DM_CTA.PessoaFJCEP.AsString, 0);
     Exped.EnderExped.UF:=DM_CTA.PessoaFJEstado.AsString;
     Exped.EnderExped.cPais:=DM_CTA.PessoaFJCodigoPais.AsInteger;
     Exped.EnderExped.xPais:=DM_CTA.PessoaFJPais.AsString;

     if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
      then begin
       Exped.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14);
       IE := DM_CTA.PessoaFJIEstadual.AsString;
      end
      else begin
       Exped.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                          Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);
       IE := 'ISENTO';
      end;

     Exped.IE:=IE;
     Exped.fone:=DM_CTA.PessoaFJTelefone.AsString;
    end;

   //
   //  Dados do Recebedor
   //
   if trim(DM_CNT.Conhec2Expedidor.AsString)<>''
    then begin
     DM_CTA.PessoaFJ.Close;
     DM_CTA.PessoaFJ.SQL.Clear;
     DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
     DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
     DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.Conhec2Expedidor.AsString;
     DM_CTA.PessoaFJ.Active:=True;
     DM_CTA.PessoaFJ.Open;

     Receb.xNome:=DM_CTA.PessoaFJRSocial.AsString;
     Receb.EnderReceb.xLgr:=DM_CTA.PessoaFJEndereco.AsString;
     Receb.EnderReceb.nro:=DM_CTA.PessoaFJNumero.AsString;
     Receb.EnderReceb.xCpl:=DM_CTA.PessoaFJComplemento.AsString;
     Receb.EnderReceb.xBairro:=DM_CTA.PessoaFJBairro.AsString;

     CodigoMunicipio:=DM_CTA.PessoaFJCodigoEstado.AsInteger * 100000 +
                      DM_CTA.PessoaFJCodigoMunicipio.AsInteger;
     Receb.EnderReceb.cMun:=CodigoMunicipio;
     Receb.EnderReceb.xMun:=DM_CTA.PessoaFJCidade.AsString;
     Receb.EnderReceb.CEP:=StrToIntDef(DM_CTA.PessoaFJCEP.AsString, 0);
     Receb.EnderReceb.UF:=DM_CTA.PessoaFJEstado.AsString;
     Receb.EnderReceb.cPais:=DM_CTA.PessoaFJCodigoPais.AsInteger;
     Receb.EnderReceb.xPais:=DM_CTA.PessoaFJPais.AsString;

     if copy(DM_CTA.PessoaFJCGC.AsString,10,4)<>'0000'
      then begin
       Receb.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 2, 14);
       IE := DM_CTA.PessoaFJIEstadual.AsString;
      end
      else begin
       Receb.CNPJCPF := Copy(DM_CTA.PessoaFJCGC.AsString, 1, 9) +
                          Copy(DM_CTA.PessoaFJCGC.AsString, 14, 2);
       IE := 'ISENTO';
      end;

     Receb.IE:=IE;
     Receb.fone:=DM_CTA.PessoaFJTelefone.AsString;
    end;
    
   //
   //  Valores da Prestação de Serviço
   //
   vPrest.vTPrest := RoundTo(DM_CNT.Conhec2ValorTotal.AsFloat, -2);
   vPrest.vRec    := RoundTo(DM_CNT.Conhec2ValorTotal.AsFloat, -2);

   //
   // Relação dos Componentes da Prestação de Serviço
   //
   DM_CNT.Componentes.Close;
   DM_CNT.Componentes.SQL.Clear;
   DM_CNT.Componentes.SQL.Add('Select * From Cnt_Componentes');
   DM_CNT.Componentes.SQL.Add('Where Codigo = :xCodigo');
   DM_CNT.Componentes.SQL.Add('and Numero = :xNumero');
   DM_CNT.Componentes.SQL.Add('Order By Item');
   DM_CNT.Componentes.Params[0].AsInteger:=DM_CNT.Conhec2Codigo.AsInteger;
   DM_CNT.Componentes.Params[1].AsInteger:=DM_CNT.Conhec2Numero.AsInteger;
   DM_CNT.Componentes.Active:=True;
   DM_CNT.Componentes.Open;
   j:=DM_CNT.Componentes.RecordCount;
   if j>0
    then begin
     DM_CNT.Componentes.First;
     for i:=1 to j do
      begin
       if DM_CNT.ComponentesValor.AsFloat>0.0
        then begin
         with vPrest.comp.Add do
          begin
           xNome:=DM_CNT.ComponentesDescricao.AsString;;
           vComp:=RoundTo(DM_CNT.ComponentesValor.AsFloat, -2);
          end;
        end;
       DM_CNT.Componentes.Next;
      end;
    end;

   //
   //  Valores dos Impostos
   //
   // TpcnCSTIcms = (cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51, cst60, cst70, cst80, cst81, cst90);
   // 80 e 81 apenas para CTe

   case DM_CNT.Conhec2CSTICMS.AsInteger of
   00: begin
        Imp.ICMS.SituTrib    := cst00;
        Imp.ICMS.CST00.CST   := cst00; // Tributação Normal ICMS
        Imp.ICMS.CST00.vBC   := RoundTo(DM_CNT.Conhec2BaseCalc.AsFloat, -2);
        Imp.ICMS.CST00.pICMS := RoundTo(DM_CNT.Conhec2AliqICMS.AsFloat, -2);
        Imp.ICMS.CST00.vICMS := RoundTo(DM_CNT.Conhec2ValorICMS.AsFloat, -2);
       end;
   20: begin
        Imp.ICMS.SituTrib     := cst20;
        Imp.ICMS.CST20.CST    := cst20; // Tributação com BC reduzida do ICMS
        Imp.ICMS.CST20.pRedBC := RoundTo(DM_CNT.Conhec2ReducaoICMS.AsFloat, -2);
        Imp.ICMS.CST20.vBC    := RoundTo(DM_CNT.Conhec2BaseCalc.AsFloat, -2);
        Imp.ICMS.CST20.pICMS  := RoundTo(DM_CNT.Conhec2AliqICMS.AsFloat, -2);
        Imp.ICMS.CST20.vICMS  := RoundTo(DM_CNT.Conhec2ValorICMS.AsFloat, -2);
       end;
   40: begin
        Imp.ICMS.SituTrib  := cst40;
        Imp.ICMS.CST45.CST := cst40; // ICMS Isento
       end;
   41: begin
        Imp.ICMS.SituTrib  := cst41;
        Imp.ICMS.CST45.CST := cst41; // ICMS não Tributada
       end;
   51: begin
        Imp.ICMS.SituTrib  := cst51;
        Imp.ICMS.CST45.CST := cst51; // ICMS diferido
       end;
   80: begin
        Imp.ICMS.SituTrib    := cst80;
        Imp.ICMS.CST80.CST   := cst90; // Tributação atribuida ao tomador ou 3. por ST
        Imp.ICMS.CST80.vBC   := RoundTo(DM_CNT.Conhec2BaseCalc.AsFloat, -2);
        Imp.ICMS.CST80.pICMS := RoundTo(DM_CNT.Conhec2AliqICMS.AsFloat, -2);
        Imp.ICMS.CST80.vICMS := RoundTo(DM_CNT.Conhec2ValorICMS.AsFloat, -2);
        Imp.ICMS.CST80.vCred := RoundTo(DM_CNT.Conhec2CreditoICMS.AsFloat, -2);
       end;
   81: begin
        Imp.ICMS.SituTrib     := cst81;
        Imp.ICMS.CST81.CST    := cst90; // Tributação devido a outra UF
        Imp.ICMS.CST81.pRedBC := RoundTo(DM_CNT.Conhec2ReducaoICMS.AsFloat, -2);
        Imp.ICMS.CST81.vBC    := RoundTo(DM_CNT.Conhec2BaseCalc.AsFloat, -2);
        Imp.ICMS.CST81.pICMS  := RoundTo(DM_CNT.Conhec2AliqICMS.AsFloat, -2);
        Imp.ICMS.CST81.vICMS  := RoundTo(DM_CNT.Conhec2ValorICMS.AsFloat, -2);
       end;
   90: begin
        Imp.ICMS.SituTrib     := cst90;
        Imp.ICMS.CST90.CST    := cst90; // ICMS Outros
        Imp.ICMS.CST90.pRedBC := RoundTo(DM_CNT.Conhec2ReducaoICMS.AsFloat, -2);
        Imp.ICMS.CST90.vBC    := RoundTo(DM_CNT.Conhec2BaseCalc.AsFloat, -2);
        Imp.ICMS.CST90.pICMS  := RoundTo(DM_CNT.Conhec2AliqICMS.AsFloat, -2);
        Imp.ICMS.CST90.vICMS  := RoundTo(DM_CNT.Conhec2ValorICMS.AsFloat, -2);
        Imp.ICMS.CST90.vCred  := RoundTo(DM_CNT.Conhec2CreditoICMS.AsFloat, -2);
       end;
   end;

   //
   //  Informações da Carga
   //
   infCarga.vMerc   := RoundTo(DM_CNT.Conhec2ValorTotalNF.AsFloat, -2);
   infCarga.proPred := DM_CNT.Conhec2Especie.AsString;
   infCarga.xOutCat := DM_CNT.Conhec2Natureza.AsString;

   // UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS);
   with infCarga.InfQ.Add do
    begin
     cUnid  := uKg;
     tpMed  := 'Kg';
     qCarga := RoundTo(DM_CNT.Conhec2PesoTotal.AsFloat, -2);
    end;
   with infCarga.InfQ.Add do
    begin
     cUnid  := uUnidade;
     tpMed  := DM_CNT.Conhec2Especie.AsString;
     qCarga := RoundTo(DM_CNT.Conhec2Volume.AsFloat, -2);
    end;

   //
   //  Informações da Seguradora
   //
   if trim(DM_CNT.ParametrosSeguradora.AsString)<>''
    then begin
     with infseg.Add do
      begin
       case DM_CNT.Conhec2RespSeguro.AsInteger of
        0: respSeg:=rsRemetente;
        1: respSeg:=rsExpedidor;
        2: respSeg:=rsRecebedor;
        3: respSeg:=rsDestinatario;
        4: respSeg:=rsEmitenteCTe;
        5: respSeg:=rsTomadorServico;
       end;

       DM_CTA.PessoaFJ.Close;
       DM_CTA.PessoaFJ.SQL.Clear;
       DM_CTA.PessoaFJ.SQL.Add('Select * From Sis_PessoaFJ');
       DM_CTA.PessoaFJ.SQL.Add('Where CGC = :xCGC');
       DM_CTA.PessoaFJ.Params[0].AsString:=DM_CNT.ParametrosSeguradora.AsString;
       DM_CTA.PessoaFJ.Active:=True;
       DM_CTA.PessoaFJ.Open;
       xSeg:=Copy(trim(DM_CTA.PessoaFJRSocial.AsString), 1, 30);

       nApol:=Copy(trim(DM_CNT.ParametrosNumApolice.AsString), 1, 20);
       nAver:=DM_CNT.ParametrosNumAverbacao.AsString;
      end;
    end;

   //
   //  Dados do Modal Rodoviário
   //
   Rodo.RNTRC:=DM_CNT.ParametrosRNTRC.AsString;
   Rodo.dPrev:=(DM_CNT.Conhec2Data.AsDateTime+1);

   // TpcteLocacao = (ltNao, ltsim);
   Rodo.Lota:=ltNao;

   //
   //  Informações do Detalhamento do CTe do tipo Anulação de Valores
   //

   // infCTeAnuEnt.dEmi:=DM_CNT.Conhec2Data.AsDateTime;
   // infCTeAnuEnt.chCTe:='';
  end;
 *)
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

procedure TfrmDemo_ACBrCTe.btnGerarPDF1Click(Sender: TObject);
begin
 OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*.xml)|*-cte.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

 if OpenDialog1.Execute then
  begin
    ACBrCTe1.InutCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirInutilizacao;
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
 Dir : string;
begin
 if Length(edtPathLogs.Text) <= 0
  then Dir := ExtractFileDir(application.ExeName)
  else Dir := edtPathLogs.Text;

 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP)
  then edtPathLogs.Text := Dir;
end;

procedure TfrmDemo_ACBrCTe.FormCreate(Sender: TObject);
begin
 LerConfiguracao;
end;

procedure TfrmDemo_ACBrCTe.btnSalvarConfigClick(Sender: TObject);
begin
 GravarConfiguracao;
 LerConfiguracao;
end;

procedure TfrmDemo_ACBrCTe.lblColaboradorClick(Sender: TObject);
begin
 OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5'); { *Converted from ShellExecute* }
end;

procedure TfrmDemo_ACBrCTe.lblPatrocinadorClick(Sender: TObject);
begin
 OpenURL('http://acbr.sourceforge.net/drupal/?q=node/35'); { *Converted from ShellExecute* }
end;

procedure TfrmDemo_ACBrCTe.lblDoar1Click(Sender: TObject);
begin
 OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14'); { *Converted from ShellExecute* }
end;

procedure TfrmDemo_ACBrCTe.ACBrCTe1StatusChange(Sender: TObject);
begin
 case ACBrCTe1.Status of
  stCTeIdle : begin
               if ( frmStatus <> nil ) then frmStatus.Hide;
              end;
  stCTeStatusServico : begin
                        if ( frmStatus = nil )
                         then frmStatus := TfrmStatus.Create(Application);
                        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
                        frmStatus.Show;
                        frmStatus.BringToFront;
                       end;
  stCTeRecepcao : begin
                   if ( frmStatus = nil )
                    then frmStatus := TfrmStatus.Create(Application);
                   frmStatus.lblStatus.Caption := 'Enviando dados do CTe...';
                   frmStatus.Show;
                   frmStatus.BringToFront;
                  end;
  stCTeRetRecepcao : begin
                      if ( frmStatus = nil )
                       then frmStatus := TfrmStatus.Create(Application);
                      frmStatus.lblStatus.Caption := 'Recebendo dados do CTe...';
                      frmStatus.Show;
                      frmStatus.BringToFront;
                     end;
  stCTeConsulta : begin
                   if ( frmStatus = nil )
                    then frmStatus := TfrmStatus.Create(Application);
                   frmStatus.lblStatus.Caption := 'Consultando CTe...';
                   frmStatus.Show;
                   frmStatus.BringToFront;
                  end;
  stCTeCancelamento : begin
                       if ( frmStatus = nil )
                        then frmStatus := TfrmStatus.Create(Application);
                       frmStatus.lblStatus.Caption := 'Enviando cancelamento de CTe...';
                       frmStatus.Show;
                       frmStatus.BringToFront;
                      end;
  stCTeInutilizacao : begin
                       if ( frmStatus = nil )
                        then frmStatus := TfrmStatus.Create(Application);
                       frmStatus.lblStatus.Caption := 'Enviando pedido de Inutilização...';
                       frmStatus.Show;
                       frmStatus.BringToFront;
                      end;
  stCTeRecibo : begin
                 if ( frmStatus = nil )
                  then frmStatus := TfrmStatus.Create(Application);
                 frmStatus.lblStatus.Caption := 'Consultando Recibo de Lote...';
                 frmStatus.Show;
                 frmStatus.BringToFront;
                end;
  stCTeCadastro : begin
                   if ( frmStatus = nil )
                    then frmStatus := TfrmStatus.Create(Application);
                   frmStatus.lblStatus.Caption := 'Consultando Cadastro...';
                   frmStatus.Show;
                   frmStatus.BringToFront;
                  end;
  {
  stCTeEnvDPEC : begin
                  if ( frmStatus = nil )
                   then frmStatus := TfrmStatus.Create(Application);
                  frmStatus.lblStatus.Caption := 'Enviando DPEC...';
                  frmStatus.Show;
                  frmStatus.BringToFront;
                 end;
  }
  stCTeEmail : begin
                if ( frmStatus = nil )
                 then frmStatus := TfrmStatus.Create(Application);
                frmStatus.lblStatus.Caption := 'Enviando Email...';
                frmStatus.Show;
                frmStatus.BringToFront;
               end;
 end;
 Application.ProcessMessages;
end;

procedure TfrmDemo_ACBrCTe.ACBrCTe1GerarLog(const Mensagem: String);
begin
 memoLog.Lines.Add(Mensagem);
end;

procedure TfrmDemo_ACBrCTe.btnStatusServClick(Sender: TObject);
begin
 ACBrCTe1.WebServices.StatusServico.Executar;
 MemoResp.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.StatusServico.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.StatusServico.RetWS);
 LoadXML(MemoResp, WBResposta);

 PageControl2.ActivePageIndex := 5;
 MemoDados.Lines.Add('');
 MemoDados.Lines.Add('Status Serviço');
 MemoDados.Lines.Add('tpAmb: '    +TpAmbToStr(ACBrCTe1.WebServices.StatusServico.tpAmb));
 MemoDados.Lines.Add('verAplic: ' +ACBrCTe1.WebServices.StatusServico.verAplic);
 MemoDados.Lines.Add('cStat: '    +IntToStr(ACBrCTe1.WebServices.StatusServico.cStat));
 MemoDados.Lines.Add('xMotivo: '  +ACBrCTe1.WebServices.StatusServico.xMotivo);
 MemoDados.Lines.Add('cUF: '      +IntToStr(ACBrCTe1.WebServices.StatusServico.cUF));
 MemoDados.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrCTe1.WebServices.StatusServico.dhRecbto));
 MemoDados.Lines.Add('tMed: '     +IntToStr(ACBrCTe1.WebServices.StatusServico.TMed));
 MemoDados.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrCTe1.WebServices.StatusServico.dhRetorno));
 MemoDados.Lines.Add('xObs: '     +ACBrCTe1.WebServices.StatusServico.xObs);
end;

procedure TfrmDemo_ACBrCTe.btnInutilizarClick(Sender: TObject);
var
 Modelo, Serie, Ano, NumeroInicial, NumeroFinal, Justificativa : String;
begin
 if not(InputQuery('WebServices Inutilização ', 'Ano',    Ano))
  then exit;
 if not(InputQuery('WebServices Inutilização ', 'Modelo', Modelo))
  then exit;
 if not(InputQuery('WebServices Inutilização ', 'Serie',  Serie))
  then exit;
 if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroInicial))
  then exit;
 if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroFinal))
  then exit;
 if not(InputQuery('WebServices Inutilização ', 'Justificativa', Justificativa))
  then exit;
 ACBrCTe1.WebServices.Inutiliza(edtEmitCNPJ.Text, Justificativa, StrToInt(Ano),
                                StrToInt(Modelo), StrToInt(Serie),
                                StrToInt(NumeroInicial), StrToInt(NumeroFinal));
 MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Inutilizacao.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Inutilizacao.RetWS);
 LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrCTe.btnGerarCTeClick(Sender: TObject);
var
 vAux : String;
begin
 if not(InputQuery('WebServices Enviar', 'Numero do Conhecimento', vAux))
  then exit;

 ACBrCTe1.Conhecimentos.Clear;
 GerarCTe(vAux);
 ACBrCTe1.Conhecimentos.Items[0].GravarXML();

 ShowMessage('Arquivo gerado em: '+ACBrCTe1.Conhecimentos.Items[0].NomeArq);
 MemoDados.Lines.Add('Arquivo gerado em: '+ACBrCTe1.Conhecimentos.Items[0].NomeArq);
 MemoResp.Lines.LoadFromFile(ACBrCTe1.Conhecimentos.Items[0].NomeArq);
 LoadXML(MemoResp, WBResposta);
 PageControl2.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrCTe.btnConsCadClick(Sender: TObject);
var
 UF, Documento : String;
begin
 if not(InputQuery('WebServices Consulta Cadastro ', 'UF do Documento a ser Consultado:', UF))
  then exit;
 if not(InputQuery('WebServices Consulta Cadastro ', 'Documento(CPF/CNPJ)', Documento))
  then exit;

 Documento := Trim(OnlyNumber(Documento));

 ACBrCTe1.WebServices.ConsultaCadastro.UF := UF;
 if Length(Documento) > 11
  then ACBrCTe1.WebServices.ConsultaCadastro.CNPJ := Documento
  else ACBrCTe1.WebServices.ConsultaCadastro.CPF := Documento;
 ACBrCTe1.WebServices.ConsultaCadastro.Executar;

 MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.ConsultaCadastro.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.ConsultaCadastro.RetWS);
 LoadXML(MemoResp, WBResposta);

 ShowMessage(ACBrCTe1.WebServices.ConsultaCadastro.xMotivo);
 ShowMessage(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xNome);
end;

procedure TfrmDemo_ACBrCTe.btnCriarEnviarClick(Sender: TObject);
var
 vAux, vNumLote : String;
begin
 if not(InputQuery('WebServices Enviar', 'Numero do Conhecimento', vAux))
  then exit;

 if not(InputQuery('WebServices Enviar', 'Numero do Lote', vNumLote))
  then exit;

 ACBrCTe1.Conhecimentos.Clear;
 GerarCTe(vAux);
 ACBrCTe1.Enviar(StrToInt(vNumLote));

 MemoResp.Lines.Text   := UTF8Encode(ACBrCTe1.WebServices.Retorno.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Retorno.RetWS);
 LoadXML(MemoResp, WBResposta);

 PageControl2.ActivePageIndex := 5;
 MemoDados.Lines.Add('');
 MemoDados.Lines.Add('Envio CTe');
 MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrCTe1.WebServices.Retorno.TpAmb));
 MemoDados.Lines.Add('verAplic: '+ ACBrCTe1.WebServices.Retorno.verAplic);
 MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrCTe1.WebServices.Retorno.cStat));
 MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrCTe1.WebServices.Retorno.cUF));
 MemoDados.Lines.Add('xMotivo: '+ ACBrCTe1.WebServices.Retorno.xMotivo);
 MemoDados.Lines.Add('xMsg: '+ ACBrCTe1.WebServices.Retorno.Msg);
 MemoDados.Lines.Add('Recibo: '+ ACBrCTe1.WebServices.Retorno.Recibo);
 MemoDados.Lines.Add('Protocolo: '+ ACBrCTe1.WebServices.Retorno.Protocolo);

 ACBrCTe1.Conhecimentos.Clear;
end;

procedure TfrmDemo_ACBrCTe.btnConsultarReciboClick(Sender: TObject);
var
 aux : String;
begin
 if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux))
  then exit;

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
   MemoResp.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Consulta.RetWS);
   memoRespWS.Lines.Text := UTF8Encode(ACBrCTe1.WebServices.Consulta.RetWS);
   LoadXML(MemoResp, WBResposta);
 end;
end;

procedure TfrmDemo_ACBrCTe.btnConsultarChaveClick(Sender: TObject);
var
 vChave : String;
begin
  if not(InputQuery('WebServices Consultar', 'Chave do CT-e:', vChave)) then
    exit;

  ACBrCTe1.WebServices.Consulta.CTeChave := vChave;
  ACBrCTe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text :=  UTF8Encode(ACBrCTe1.WebServices.Consulta.RetWS);
  memoRespWS.Lines.Text :=  UTF8Encode(ACBrCTe1.WebServices.Consulta.RetornoWS);
  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrCTe.btnEnvEPECClick(Sender: TObject);
var
 vAux : String;
begin
 ShowMessage('Opção não Implementada, no programa exemplo!');
end;

procedure TfrmDemo_ACBrCTe.btnImportarXMLClick(Sender: TObject);
var
 i, j, k, n  : integer;
 Nota, Node, NodePai, NodeItem: TTreeNode;
begin
 OpenDialog1.FileName  :=  '';
 OpenDialog1.Title := 'Selecione o CTe';
 OpenDialog1.DefaultExt := '*-cte.xml';
 OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

 if OpenDialog1.Execute then
  begin
   ACBrCTe1.Conhecimentos.Clear;
   ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
   trvwCTe.Items.Clear;

   for n:=0 to ACBrCTe1.Conhecimentos.Count-1 do
    begin
     with ACBrCTe1.Conhecimentos.Items[n].CTe do
      begin
       (*
       Nota := trvwCTe.Items.Add(nil,infCTe.ID);
       trvwCTe.Items.AddChild(Nota,'ID= ' +infCTe.ID);
       Node := trvwCTe.Items.AddChild(Nota,'procCTe');
       trvwCTe.Items.AddChild(Node,'tpAmb= '     +TpAmbToStr(procCTe.tpAmb));
       trvwCTe.Items.AddChild(Node,'verAplic= '  +procCTe.verAplic);
       trvwCTe.Items.AddChild(Node,'chCTe= '     +procCTe.chCTe);
       trvwCTe.Items.AddChild(Node,'dhRecbto= '  +DateTimeToStr(procCTe.dhRecbto));
       trvwCTe.Items.AddChild(Node,'nProt= '     +procCTe.nProt);
       trvwCTe.Items.AddChild(Node,'digVal= '    +procCTe.digVal);
       trvwCTe.Items.AddChild(Node,'cStat= '     +IntToStr(procCTe.cStat));
       trvwCTe.Items.AddChild(Node,'xMotivo= '   +procCTe.xMotivo);

       Node := trvwCTe.Items.AddChild(Nota,'Ide');
       trvwCTe.Items.AddChild(Node,'cNF= '     +IntToStr(Ide.cNF));
       trvwCTe.Items.AddChild(Node,'natOp= '   +Ide.natOp );
       trvwCTe.Items.AddChild(Node,'indPag= '  +IndpagToStr(Ide.indPag));
       trvwCTe.Items.AddChild(Node,'modelo= '  +IntToStr(Ide.modelo));
       trvwCTe.Items.AddChild(Node,'serie= '   +IntToStr(Ide.serie));
       trvwCTe.Items.AddChild(Node,'nNF= '     +IntToStr(Ide.nNF));
       trvwCTe.Items.AddChild(Node,'dEmi= '    +DateToStr(Ide.dEmi));
       trvwCTe.Items.AddChild(Node,'dSaiEnt= ' +DateToStr(Ide.dSaiEnt));
       trvwCTe.Items.AddChild(Node,'tpNF= '    +tpNFToStr(Ide.tpNF));
       trvwCTe.Items.AddChild(Node,'finCTe= '  +FinCTeToStr(Ide.finCTe));
       trvwCTe.Items.AddChild(Node,'verProc= ' +Ide.verProc);
       trvwCTe.Items.AddChild(Node,'cUF= '     +IntToStr(Ide.cUF));
       trvwCTe.Items.AddChild(Node,'cMunFG= '  +IntToStr(Ide.cMunFG));
       trvwCTe.Items.AddChild(Node,'tpImp= '   +TpImpToStr(Ide.tpImp));
       trvwCTe.Items.AddChild(Node,'tpEmis= '  +TpEmisToStr(Ide.tpEmis));
       trvwCTe.Items.AddChild(Node,'cDV= '     +IntToStr(Ide.cDV));
       trvwCTe.Items.AddChild(Node,'tpAmb= '   +TpAmbToStr(Ide.tpAmb));
       trvwCTe.Items.AddChild(Node,'finCTe= '  +FinCTeToStr(Ide.finCTe));
       trvwCTe.Items.AddChild(Node,'procEmi= ' +procEmiToStr(Ide.procEmi));
       trvwCTe.Items.AddChild(Node,'verProc= ' +Ide.verProc);

       for i:=0 to Ide.NFref.Count-1 do
        begin
          Node := trvwCTe.Items.AddChild(Node,'NFRef'+IntToStrZero(i+1,3));
          trvwCTe.Items.AddChild(Node,'refCTe= ' +Ide.NFref.Items[i].refCTe);
          trvwCTe.Items.AddChild(Node,'cUF= '    +IntToStr(Ide.NFref.Items[i].RefNF.cUF));
          trvwCTe.Items.AddChild(Node,'AAMM= '   +Ide.NFref.Items[i].RefNF.AAMM);
          trvwCTe.Items.AddChild(Node,'CNPJ= '   +Ide.NFref.Items[i].RefNF.CNPJ);
          trvwCTe.Items.AddChild(Node,'modelo= ' +IntToStr(Ide.NFref.Items[i].RefNF.modelo));
          trvwCTe.Items.AddChild(Node,'serie= '  +IntToStr(Ide.NFref.Items[i].RefNF.serie));
          trvwCTe.Items.AddChild(Node,'nNF= '    +IntToStr(Ide.NFref.Items[i].RefNF.nNF));
        end;

       Node := trvwCTe.Items.AddChild(Nota,'Emit');
       trvwCTe.Items.AddChild(Node,'CNPJCPF= ' +Emit.CNPJCPF);
       trvwCTe.Items.AddChild(Node,'IE='       +Emit.IE);
       trvwCTe.Items.AddChild(Node,'xNome='    +Emit.xNome);
       trvwCTe.Items.AddChild(Node,'xFant='    +Emit.xFant );
       trvwCTe.Items.AddChild(Node,'IEST='     +Emit.IEST);
       trvwCTe.Items.AddChild(Node,'IM='       +Emit.IM);
       trvwCTe.Items.AddChild(Node,'CNAE='     +Emit.CNAE);

       Node := trvwCTe.Items.AddChild(Node,'EnderEmit');
       trvwCTe.Items.AddChild(Node,'Fone='    +Emit.EnderEmit.fone);
       trvwCTe.Items.AddChild(Node,'CEP='     +IntToStr(Emit.EnderEmit.CEP));
       trvwCTe.Items.AddChild(Node,'xLgr='    +Emit.EnderEmit.xLgr);
       trvwCTe.Items.AddChild(Node,'nro='     +Emit.EnderEmit.nro);
       trvwCTe.Items.AddChild(Node,'xCpl='    +Emit.EnderEmit.xCpl);
       trvwCTe.Items.AddChild(Node,'xBairro=' +Emit.EnderEmit.xBairro);
       trvwCTe.Items.AddChild(Node,'cMun='    +IntToStr(Emit.EnderEmit.cMun));
       trvwCTe.Items.AddChild(Node,'xMun='    +Emit.EnderEmit.xMun);
       trvwCTe.Items.AddChild(Node,'UF'       +Emit.EnderEmit.UF);
       trvwCTe.Items.AddChild(Node,'cPais='   +IntToStr(Emit.EnderEmit.cPais));
       trvwCTe.Items.AddChild(Node,'xPais='   +Emit.EnderEmit.xPais);

       if Avulsa.CNPJ  <> '' then
        begin
          Node := trvwCTe.Items.AddChild(Nota,'Avulsa');
          trvwCTe.Items.AddChild(Node,'CNPJ='    +Avulsa.CNPJ);
          trvwCTe.Items.AddChild(Node,'xOrgao='  +Avulsa.xOrgao);
          trvwCTe.Items.AddChild(Node,'matr='    +Avulsa.matr );
          trvwCTe.Items.AddChild(Node,'xAgente=' +Avulsa.xAgente);
          trvwCTe.Items.AddChild(Node,'fone='    +Avulsa.fone);
          trvwCTe.Items.AddChild(Node,'UF='      +Avulsa.UF);
          trvwCTe.Items.AddChild(Node,'nDAR='    +Avulsa.nDAR);
          trvwCTe.Items.AddChild(Node,'dEmi='    +DateToStr(Avulsa.dEmi));
          trvwCTe.Items.AddChild(Node,'vDAR='    +FloatToStr(Avulsa.vDAR));
          trvwCTe.Items.AddChild(Node,'repEmi='  +Avulsa.repEmi);
          trvwCTe.Items.AddChild(Node,'dPag='    +DateToStr(Avulsa.dPag));
        end;
       Node := trvwCTe.Items.AddChild(Nota,'Dest');
       trvwCTe.Items.AddChild(Node,'CNPJCPF= ' +Dest.CNPJCPF);
       trvwCTe.Items.AddChild(Node,'IE='       +Dest.IE);
       trvwCTe.Items.AddChild(Node,'ISUF='     +Dest.ISUF);
       trvwCTe.Items.AddChild(Node,'xNome='    +Dest.xNome);

       Node := trvwCTe.Items.AddChild(Node,'EnderDest');
       trvwCTe.Items.AddChild(Node,'Fone='    +Dest.EnderDest.Fone);
       trvwCTe.Items.AddChild(Node,'CEP='     +IntToStr(Dest.EnderDest.CEP));
       trvwCTe.Items.AddChild(Node,'xLgr='    +Dest.EnderDest.xLgr);
       trvwCTe.Items.AddChild(Node,'nro='     +Dest.EnderDest.nro);
       trvwCTe.Items.AddChild(Node,'xCpl='    +Dest.EnderDest.xCpl);
       trvwCTe.Items.AddChild(Node,'xBairro=' +Dest.EnderDest.xBairro);
       trvwCTe.Items.AddChild(Node,'cMun='    +IntToStr(Dest.EnderDest.cMun));
       trvwCTe.Items.AddChild(Node,'xMun='    +Dest.EnderDest.xMun);
       trvwCTe.Items.AddChild(Node,'UF='      +Dest.EnderDest.UF );
       trvwCTe.Items.AddChild(Node,'cPais='   +IntToStr(Dest.EnderDest.cPais));
       trvwCTe.Items.AddChild(Node,'xPais='   +Dest.EnderDest.xPais);

       {if Retirada.CNPJ <> '' then
        begin
          Node := trvwCTe.Items.AddChild(Nota,'Retirada');
          trvwCTe.Items.AddChild(Node,'CNPJ='    +Retirada.CNPJ);
          trvwCTe.Items.AddChild(Node,'xLgr='    +Retirada.xLgr);
          trvwCTe.Items.AddChild(Node,'nro='     +Retirada.nro);
          trvwCTe.Items.AddChild(Node,'xCpl='    +Retirada.xCpl);
          trvwCTe.Items.AddChild(Node,'xBairro=' +Retirada.xBairro);
          trvwCTe.Items.AddChild(Node,'cMun='    +IntToStr(Retirada.cMun));
          trvwCTe.Items.AddChild(Node,'xMun='    +Retirada.xMun);
          trvwCTe.Items.AddChild(Node,'UF='      +Retirada.UF);
        end;

       if Entrega.CNPJ <> '' then
        begin
          Node := trvwCTe.Items.AddChild(Nota,'Entrega');
          trvwCTe.Items.AddChild(Node,'CNPJ='    +Entrega.CNPJ);
          trvwCTe.Items.AddChild(Node,'xLgr='    +Entrega.xLgr);
          trvwCTe.Items.AddChild(Node,'nro='     +Entrega.nro);
          trvwCTe.Items.AddChild(Node,'xCpl='    +Entrega.xCpl);
          trvwCTe.Items.AddChild(Node,'xBairro=' +Entrega.xBairro);
          trvwCTe.Items.AddChild(Node,'cMun='    +IntToStr(Entrega.cMun));
          trvwCTe.Items.AddChild(Node,'xMun='    +Entrega.xMun);
          trvwCTe.Items.AddChild(Node,'UF='      +Entrega.UF);
        end;}

       for I := 0 to Det.Count-1 do
        begin
          with Det.Items[I] do
           begin
               NodeItem := trvwCTe.Items.AddChild(Nota,'Produto'+IntToStrZero(I+1,3));
               trvwCTe.Items.AddChild(NodeItem,'nItem='  +IntToStr(Prod.nItem) );
               trvwCTe.Items.AddChild(NodeItem,'cProd='  +Prod.cProd );
               trvwCTe.Items.AddChild(NodeItem,'cEAN='   +Prod.cEAN);
               trvwCTe.Items.AddChild(NodeItem,'xProd='  +Prod.xProd);
               trvwCTe.Items.AddChild(NodeItem,'NCM='    +Prod.NCM);
               trvwCTe.Items.AddChild(NodeItem,'EXTIPI=' +Prod.EXTIPI);
               //trvwCTe.Items.AddChild(NodeItem,'genero=' +IntToStr(Prod.genero));
               trvwCTe.Items.AddChild(NodeItem,'CFOP='   +Prod.CFOP);
               trvwCTe.Items.AddChild(NodeItem,'uCom='   +Prod.uCom);
               trvwCTe.Items.AddChild(NodeItem,'qCom='   +FloatToStr(Prod.qCom));
               trvwCTe.Items.AddChild(NodeItem,'vUnCom=' +FloatToStr(Prod.vUnCom));
               trvwCTe.Items.AddChild(NodeItem,'vProd='  +FloatToStr(Prod.vProd));

               trvwCTe.Items.AddChild(NodeItem,'cEANTrib=' +Prod.cEANTrib);
               trvwCTe.Items.AddChild(NodeItem,'uTrib='    +Prod.uTrib);
               trvwCTe.Items.AddChild(NodeItem,'qTrib='    +FloatToStr(Prod.qTrib));
               trvwCTe.Items.AddChild(NodeItem,'vUnTrib='  +FloatToStr(Prod.vUnTrib));

               trvwCTe.Items.AddChild(NodeItem,'vFrete=' +FloatToStr(Prod.vFrete));
               trvwCTe.Items.AddChild(NodeItem,'vSeg='   +FloatToStr(Prod.vSeg));
               trvwCTe.Items.AddChild(NodeItem,'vDesc='  +FloatToStr(Prod.vDesc));

               trvwCTe.Items.AddChild(NodeItem,'infAdProd=' +infAdProd);

               for J:=0 to Prod.DI.Count-1 do
                begin
                  if Prod.DI.Items[j].nDi <> '' then
                   begin
                     with Prod.DI.Items[j] do
                      begin
                        NodePai := trvwCTe.Items.AddChild(NodeItem,'DI'+IntToStrZero(J+1,3));
                        trvwCTe.Items.AddChild(NodePai,'nDi='         +nDi);
                        trvwCTe.Items.AddChild(NodePai,'dDi='         +DateToStr(dDi));
                        trvwCTe.Items.AddChild(NodePai,'xLocDesemb='  +xLocDesemb);
                        trvwCTe.Items.AddChild(NodePai,'UFDesemb='    +UFDesemb);
                        trvwCTe.Items.AddChild(NodePai,'dDesemb='     +DateToStr(dDesemb));
                        trvwCTe.Items.AddChild(NodePai,'cExportador=' +cExportador);;

                        for K:=0 to adi.Count-1 do
                         begin
                           with adi.Items[K] do
                            begin
                              Node := trvwCTe.Items.AddChild(NodePai,'LADI'+IntToStrZero(K+1,3));
                              trvwCTe.Items.AddChild(Node,'nAdicao='     +IntToStr(nAdicao));
                              trvwCTe.Items.AddChild(Node,'nSeqAdi='     +IntToStr(nSeqAdi));
                              trvwCTe.Items.AddChild(Node,'cFabricante=' +cFabricante);
                              trvwCTe.Items.AddChild(Node,'vDescDI='     +FloatToStr(vDescDI));
                            end;
                         end;
                      end;
                   end
                  else
                    Break;
                end;

              if Prod.veicProd.chassi <> '' then
               begin
                 Node := trvwCTe.Items.AddChild(NodeItem,'Veiculo');
                 with Prod.veicProd do
                  begin
                    trvwCTe.Items.AddChild(Node,'tpOP='     +tpOPToStr(tpOP));
                    trvwCTe.Items.AddChild(Node,'chassi='   +chassi);
                    trvwCTe.Items.AddChild(Node,'cCor='     +cCor);
                    trvwCTe.Items.AddChild(Node,'xCor='     +xCor);
                    trvwCTe.Items.AddChild(Node,'pot='      +pot);
                    trvwCTe.Items.AddChild(Node,'Cilin='      +Cilin);
                    trvwCTe.Items.AddChild(Node,'pesoL='    +pesoL);
                    trvwCTe.Items.AddChild(Node,'pesoB='    +pesoB);
                    trvwCTe.Items.AddChild(Node,'nSerie='   +nSerie);
                    trvwCTe.Items.AddChild(Node,'tpComb='   +tpComb);
                    trvwCTe.Items.AddChild(Node,'nMotor='   +nMotor);
                    trvwCTe.Items.AddChild(Node,'CMT='     +CMT);
                    trvwCTe.Items.AddChild(Node,'dist='     +dist);
                    trvwCTe.Items.AddChild(Node,'RENAVAM='  +RENAVAM);
                    trvwCTe.Items.AddChild(Node,'anoMod='   +IntToStr(anoMod));
                    trvwCTe.Items.AddChild(Node,'anoFab='   +IntToStr(anoFab));
                    trvwCTe.Items.AddChild(Node,'tpPint='   +tpPint);
                    trvwCTe.Items.AddChild(Node,'tpVeic='   +IntToStr(tpVeic));
                    trvwCTe.Items.AddChild(Node,'espVeic='  +IntToStr(espVeic));
                    trvwCTe.Items.AddChild(Node,'VIN='      +VIN);
                    trvwCTe.Items.AddChild(Node,'condVeic=' +condVeicToStr(condVeic));
                    trvwCTe.Items.AddChild(Node,'cMod='     +cMod);
                  end;
               end;

               for J:=0 to Prod.med.Count-1 do
                begin
                  Node := trvwCTe.Items.AddChild(NodeItem,'Medicamento'+IntToStrZero(J+1,3) );
                  with Prod.med.Items[J] do
                   begin
                     trvwCTe.Items.AddChild(Node,'nLote=' +nLote);
                     trvwCTe.Items.AddChild(Node,'qLote=' +FloatToStr(qLote));
                     trvwCTe.Items.AddChild(Node,'dFab='  +DateToStr(dFab));
                     trvwCTe.Items.AddChild(Node,'dVal='  +DateToStr(dVal));
                     trvwCTe.Items.AddChild(Node,'vPMC='  +FloatToStr(vPMC));
                    end;
                end;

               for J:=0 to Prod.arma.Count-1 do
                begin
                  Node := trvwCTe.Items.AddChild(NodeItem,'Arma'+IntToStrZero(J+1,3));
                  with Prod.arma.Items[J] do
                   begin
                     trvwCTe.Items.AddChild(Node,'nSerie=' +IntToStr(nSerie));
                     trvwCTe.Items.AddChild(Node,'tpArma=' +tpArmaToStr(tpArma));
                     trvwCTe.Items.AddChild(Node,'nCano='  +IntToStr(nCano));
                     trvwCTe.Items.AddChild(Node,'descr='  +descr);
                    end;
                end;

               if (Prod.comb.cProdANP > 0) then
                begin
                 NodePai := trvwCTe.Items.AddChild(NodeItem,'Combustivel');
                 with Prod.comb do
                  begin
                    trvwCTe.Items.AddChild(NodePai,'cProdANP=' +IntToStr(cProdANP));
                    trvwCTe.Items.AddChild(NodePai,'CODIF='    +CODIF);
                    trvwCTe.Items.AddChild(NodePai,'qTemp='    +FloatToStr(qTemp));

                    Node := trvwCTe.Items.AddChild(NodePai,'CIDE'+IntToStrZero(I+1,3));
                    trvwCTe.Items.AddChild(Node,'qBCprod='   +FloatToStr(CIDE.qBCprod));
                    trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(CIDE.vAliqProd));
                    trvwCTe.Items.AddChild(Node,'vCIDE='     +FloatToStr(CIDE.vCIDE));

                    Node := trvwCTe.Items.AddChild(NodePai,'ICMSComb'+IntToStrZero(I+1,3));
                    trvwCTe.Items.AddChild(Node,'vBCICMS='   +FloatToStr(ICMS.vBCICMS));
                    trvwCTe.Items.AddChild(Node,'vICMS='     +FloatToStr(ICMS.vICMS));
                    trvwCTe.Items.AddChild(Node,'vBCICMSST=' +FloatToStr(ICMS.vBCICMSST));
                    trvwCTe.Items.AddChild(Node,'vICMSST='   +FloatToStr(ICMS.vICMSST));

                    if (ICMSInter.vBCICMSSTDest>0) then
                     begin
                       Node := trvwCTe.Items.AddChild(NodePai,'ICMSInter'+IntToStrZero(I+1,3));
                       trvwCTe.Items.AddChild(Node,'vBCICMSSTDest=' +FloatToStr(ICMSInter.vBCICMSSTDest));
                       trvwCTe.Items.AddChild(Node,'vICMSSTDest='   +FloatToStr(ICMSInter.vICMSSTDest));
                     end;

                    if (ICMSCons.vBCICMSSTCons>0) then
                     begin
                       Node := trvwCTe.Items.AddChild(NodePai,'ICMSCons'+IntToStrZero(I+1,3));
                       trvwCTe.Items.AddChild(Node,'vBCICMSSTCons=' +FloatToStr(ICMSCons.vBCICMSSTCons));
                       trvwCTe.Items.AddChild(Node,'vICMSSTCons='   +FloatToStr(ICMSCons.vICMSSTCons));
                       trvwCTe.Items.AddChild(Node,'UFCons='        +ICMSCons.UFcons);
                     end;
                  end;
               end;

               with Imposto do
                begin
                   NodePai := trvwCTe.Items.AddChild(NodeItem,'Imposto');
                   Node := trvwCTe.Items.AddChild(NodePai,'ICMS');
                   with ICMS do
                    begin
                      trvwCTe.Items.AddChild(Node,'CST=' +CSTICMSToStr(CST));

                      if CST = cst00 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='  +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBC=' +modBCToStr(ICMS.modBC));
                         trvwCTe.Items.AddChild(Node,'vBC='   +FloatToStr(ICMS.vBC));
                         trvwCTe.Items.AddChild(Node,'pICMS=' +FloatToStr(ICMS.pICMS));
                         trvwCTe.Items.AddChild(Node,'vICMS=' +FloatToStr(ICMS.vICMS));
                       end
                      else if CST = cst10 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='     +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBC='    +modBCToStr(ICMS.modBC));
                         trvwCTe.Items.AddChild(Node,'vBC='      +FloatToStr(ICMS.vBC));
                         trvwCTe.Items.AddChild(Node,'pICMS='    +FloatToStr(ICMS.pICMS));
                         trvwCTe.Items.AddChild(Node,'vICMS='    +FloatToStr(ICMS.vICMS));
                         trvwCTe.Items.AddChild(Node,'modBCST='  +modBCSTToStr(ICMS.modBCST));
                         trvwCTe.Items.AddChild(Node,'pMVAST='   +FloatToStr(ICMS.pMVAST));
                         trvwCTe.Items.AddChild(Node,'pRedBCST=' +FloatToStr(ICMS.pRedBCST));
                         trvwCTe.Items.AddChild(Node,'vBCST='    +FloatToStr(ICMS.vBCST));
                         trvwCTe.Items.AddChild(Node,'pICMSST='  +FloatToStr(ICMS.pICMSST));
                         trvwCTe.Items.AddChild(Node,'vICMSST='  +FloatToStr(ICMS.vICMSST));
                       end
                      else if CST = cst20 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='   +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBC='  +modBCToStr(ICMS.modBC));
                         trvwCTe.Items.AddChild(Node,'pRedBC=' +FloatToStr(ICMS.pRedBC));
                         trvwCTe.Items.AddChild(Node,'vBC='    +FloatToStr(ICMS.vBC));
                         trvwCTe.Items.AddChild(Node,'pICMS='  +FloatToStr(ICMS.pICMS));
                         trvwCTe.Items.AddChild(Node,'vICMS='  +FloatToStr(ICMS.vICMS));
                       end
                      else if CST = cst30 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='     +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBCST='  +modBCSTToStr(ICMS.modBCST));
                         trvwCTe.Items.AddChild(Node,'pMVAST='   +FloatToStr(ICMS.pMVAST));
                         trvwCTe.Items.AddChild(Node,'pRedBCST=' +FloatToStr(ICMS.pRedBCST));
                         trvwCTe.Items.AddChild(Node,'vBCST='    +FloatToStr(ICMS.vBCST));
                         trvwCTe.Items.AddChild(Node,'pICMSST='  +FloatToStr(ICMS.pICMSST));
                         trvwCTe.Items.AddChild(Node,'vICMSST='  +FloatToStr(ICMS.vICMSST));
                       end
                      else if (CST = cst40) or (CST = cst41) or (CST = cst50) then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='    +OrigToStr(ICMS.orig));
                       end
                      else if CST = cst51 then
                         begin
                         trvwCTe.Items.AddChild(Node,'orig='    +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBC='   +modBCToStr(ICMS.modBC));
                         trvwCTe.Items.AddChild(Node,'pRedBC='  +FloatToStr(ICMS.pRedBC));
                         trvwCTe.Items.AddChild(Node,'vBC='     +FloatToStr(ICMS.vBC));
                         trvwCTe.Items.AddChild(Node,'pICMS='   +FloatToStr(ICMS.pICMS));
                         trvwCTe.Items.AddChild(Node,'vICMS='   +FloatToStr(ICMS.vICMS));
                       end
                      else if CST = cst60 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='    +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'vBCST='   +FloatToStr(ICMS.vBCST));
                         trvwCTe.Items.AddChild(Node,'vICMSST=' +FloatToStr(ICMS.vICMSST));
                       end
                      else if CST = cst70 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='       +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBC='      +modBCToStr(ICMS.modBC));
                         trvwCTe.Items.AddChild(Node,'pRedBC='     +FloatToStr(ICMS.pRedBC));
                         trvwCTe.Items.AddChild(Node,'vBC='        +FloatToStr(ICMS.vBC));
                         trvwCTe.Items.AddChild(Node,'pICMS='      +FloatToStr(ICMS.pICMS));
                         trvwCTe.Items.AddChild(Node,'vICMS='      +FloatToStr(ICMS.vICMS));
                         trvwCTe.Items.AddChild(Node,'modBCST='    +modBCSTToStr(ICMS.modBCST));
                         trvwCTe.Items.AddChild(Node,'pMVAST='     +FloatToStr(ICMS.pMVAST));
                         trvwCTe.Items.AddChild(Node,'pRedBCST='   +FloatToStr(ICMS.pRedBCST));
                         trvwCTe.Items.AddChild(Node,'vBCST='      +FloatToStr(ICMS.vBCST));
                         trvwCTe.Items.AddChild(Node,'pICMSST='    +FloatToStr(ICMS.pICMSST));
                         trvwCTe.Items.AddChild(Node,'vICMSST='    +FloatToStr(ICMS.vICMSST));
                       end
                      else if CST = cst90 then
                       begin
                         trvwCTe.Items.AddChild(Node,'orig='       +OrigToStr(ICMS.orig));
                         trvwCTe.Items.AddChild(Node,'modBC='      +modBCToStr(ICMS.modBC));
                         trvwCTe.Items.AddChild(Node,'pRedBC='     +FloatToStr(ICMS.pRedBC));
                         trvwCTe.Items.AddChild(Node,'vBC='        +FloatToStr(ICMS.vBC));
                         trvwCTe.Items.AddChild(Node,'pICMS='      +FloatToStr(ICMS.pICMS));
                         trvwCTe.Items.AddChild(Node,'vICMS='      +FloatToStr(ICMS.vICMS));
                         trvwCTe.Items.AddChild(Node,'modBCST='    +modBCSTToStr(ICMS.modBCST));
                         trvwCTe.Items.AddChild(Node,'pMVAST='     +FloatToStr(ICMS.pMVAST));
                         trvwCTe.Items.AddChild(Node,'pRedBCST='   +FloatToStr(ICMS.pRedBCST));
                         trvwCTe.Items.AddChild(Node,'vBCST='      +FloatToStr(ICMS.vBCST));
                         trvwCTe.Items.AddChild(Node,'pICMSST='    +FloatToStr(ICMS.pICMSST));
                         trvwCTe.Items.AddChild(Node,'vICMSST='    +FloatToStr(ICMS.vICMSST));
                       end;
                    end;

                   if (IPI.vBC > 0) then
                    begin
                      Node := trvwCTe.Items.AddChild(NodePai,'IPI');
                      with IPI do
                       begin
                         trvwCTe.Items.AddChild(Node,'CST='       +CSTIPIToStr(CST));
                         trvwCTe.Items.AddChild(Node,'clEnq='    +clEnq);
                         trvwCTe.Items.AddChild(Node,'CNPJProd=' +CNPJProd);
                         trvwCTe.Items.AddChild(Node,'cSelo='    +cSelo);
                         trvwCTe.Items.AddChild(Node,'qSelo='    +IntToStr(qSelo));
                         trvwCTe.Items.AddChild(Node,'cEnq='     +cEnq);

                         trvwCTe.Items.AddChild(Node,'vBC='    +FloatToStr(vBC));
                         trvwCTe.Items.AddChild(Node,'qUnid='  +FloatToStr(qUnid));
                         trvwCTe.Items.AddChild(Node,'vUnid='  +FloatToStr(vUnid));
                         trvwCTe.Items.AddChild(Node,'pIPI='   +FloatToStr(pIPI));
                         trvwCTe.Items.AddChild(Node,'vIPI='   +FloatToStr(vIPI));
                       end;
                    end;

                   if (II.vBc > 0) then
                    begin
                      Node := trvwCTe.Items.AddChild(NodePai,'II');
                      with II do
                       begin
                         trvwCTe.Items.AddChild(Node,'vBc='      +FloatToStr(vBc));
                         trvwCTe.Items.AddChild(Node,'vDespAdu=' +FloatToStr(vDespAdu));
                         trvwCTe.Items.AddChild(Node,'vII='      +FloatToStr(vII));
                         trvwCTe.Items.AddChild(Node,'vIOF='     +FloatToStr(vIOF));
                       end;
                    end;

                   Node := trvwCTe.Items.AddChild(NodePai,'PIS');
                   with PIS do
                    begin
                      trvwCTe.Items.AddChild(Node,'CST=' +CSTPISToStr(CST));

                      if (CST = pis01) or (CST = pis02) then
                       begin
                         trvwCTe.Items.AddChild(Node,'vBC='  +FloatToStr(PIS.vBC));
                         trvwCTe.Items.AddChild(Node,'pPIS=' +FloatToStr(PIS.pPIS));
                         trvwCTe.Items.AddChild(Node,'vPIS=' +FloatToStr(PIS.vPIS));
                       end
                      else if CST = pis03 then
                       begin
                         trvwCTe.Items.AddChild(Node,'qBCProd='   +FloatToStr(PIS.qBCProd));
                         trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(PIS.vAliqProd));
                         trvwCTe.Items.AddChild(Node,'vPIS='      +FloatToStr(PIS.vPIS));
                       end
                      else if CST = pis99 then
                       begin
                         trvwCTe.Items.AddChild(Node,'vBC='       +FloatToStr(PIS.vBC));
                         trvwCTe.Items.AddChild(Node,'pPIS='      +FloatToStr(PIS.pPIS));
                         trvwCTe.Items.AddChild(Node,'qBCProd='   +FloatToStr(PIS.qBCProd));
                         trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(PIS.vAliqProd));
                         trvwCTe.Items.AddChild(Node,'vPIS='      +FloatToStr(PIS.vPIS));
                       end;
                    end;

                   if (PISST.vBc>0) then
                    begin
                      Node := trvwCTe.Items.AddChild(NodePai,'PISST');
                      with PISST do
                       begin
                         trvwCTe.Items.AddChild(Node,'vBc='       +FloatToStr(vBc));
                         trvwCTe.Items.AddChild(Node,'pPis='      +FloatToStr(pPis));
                         trvwCTe.Items.AddChild(Node,'qBCProd='   +FloatToStr(qBCProd));
                         trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(vAliqProd));
                         trvwCTe.Items.AddChild(Node,'vPIS='      +FloatToStr(vPIS));
                       end;
                      end;

                   Node := trvwCTe.Items.AddChild(NodePai,'COFINS');
                   with COFINS do
                    begin
                      trvwCTe.Items.AddChild(Node,'CST=' +CSTCOFINSToStr(CST));

                      if (CST = cof01) or (CST = cof02)   then
                       begin
                         trvwCTe.Items.AddChild(Node,'vBC='     +FloatToStr(COFINS.vBC));
                         trvwCTe.Items.AddChild(Node,'pCOFINS=' +FloatToStr(COFINS.pCOFINS));
                         trvwCTe.Items.AddChild(Node,'vCOFINS=' +FloatToStr(COFINS.vCOFINS));
                       end
                      else if CST = cof03 then
                       begin
                         trvwCTe.Items.AddChild(Node,'qBCProd='   +FloatToStr(COFINS.qBCProd));
                         trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(COFINS.vAliqProd));
                         trvwCTe.Items.AddChild(Node,'vCOFINS='   +FloatToStr(COFINS.vCOFINS));
                       end
                      else if CST = cof99 then
                       begin
                         trvwCTe.Items.AddChild(Node,'vBC='       +FloatToStr(COFINS.vBC));
                         trvwCTe.Items.AddChild(Node,'pCOFINS='   +FloatToStr(COFINS.pCOFINS));
                         trvwCTe.Items.AddChild(Node,'qBCProd='   +FloatToStr(COFINS.qBCProd));
                         trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(COFINS.vAliqProd));
                         trvwCTe.Items.AddChild(Node,'vCOFINS='   +FloatToStr(COFINS.vCOFINS));
                       end;
                    end;

                   if (COFINSST.vBC > 0) then
                    begin
                      Node := trvwCTe.Items.AddChild(NodePai,'COFINSST');
                      with COFINSST do
                       begin
                         trvwCTe.Items.AddChild(Node,'vBC='       +FloatToStr(vBC));
                         trvwCTe.Items.AddChild(Node,'pCOFINS='   +FloatToStr(pCOFINS));
                         trvwCTe.Items.AddChild(Node,'qBCProd='   +FloatToStr(qBCProd));
                         trvwCTe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(vAliqProd));
                         trvwCTe.Items.AddChild(Node,'vCOFINS='   +FloatToStr(vCOFINS));
                       end;
                    end;

                   if (ISSQN.vBC > 0) then
                    begin
                     Node := trvwCTe.Items.AddChild(NodePai,'ISSQN');
                     with ISSQN do
                      begin
                        trvwCTe.Items.AddChild(Node,'vBC='       +FloatToStr(vBC));
                        trvwCTe.Items.AddChild(Node,'vAliq='     +FloatToStr(vAliq));
                        trvwCTe.Items.AddChild(Node,'vISSQN='    +FloatToStr(vISSQN));
                        trvwCTe.Items.AddChild(Node,'cMunFG='    +IntToStr(cMunFG));
                        trvwCTe.Items.AddChild(Node,'cListServ=' +IntToStr(cListServ));
                      end;
                    end;
                end;
             end;
          end;

       NodePai := trvwCTe.Items.AddChild(Nota,'Total');
       Node := trvwCTe.Items.AddChild(NodePai,'ICMSTot');
       trvwCTe.Items.AddChild(Node,'vBC='     +FloatToStr(Total.ICMSTot.vBC));
       trvwCTe.Items.AddChild(Node,'vICMS='   +FloatToStr(Total.ICMSTot.vICMS));
       trvwCTe.Items.AddChild(Node,'vBCST='   +FloatToStr(Total.ICMSTot.vBCST));
       trvwCTe.Items.AddChild(Node,'vST='     +FloatToStr(Total.ICMSTot.vST));
       trvwCTe.Items.AddChild(Node,'vProd='   +FloatToStr(Total.ICMSTot.vProd));
       trvwCTe.Items.AddChild(Node,'vFrete='  +FloatToStr(Total.ICMSTot.vFrete));
       trvwCTe.Items.AddChild(Node,'vSeg='    +FloatToStr(Total.ICMSTot.vSeg));
       trvwCTe.Items.AddChild(Node,'vDesc='   +FloatToStr(Total.ICMSTot.vDesc));
       trvwCTe.Items.AddChild(Node,'vII='     +FloatToStr(Total.ICMSTot.vII));
       trvwCTe.Items.AddChild(Node,'vIPI='    +FloatToStr(Total.ICMSTot.vIPI));
       trvwCTe.Items.AddChild(Node,'vPIS='    +FloatToStr(Total.ICMSTot.vPIS));
       trvwCTe.Items.AddChild(Node,'vCOFINS=' +FloatToStr(Total.ICMSTot.vCOFINS));
       trvwCTe.Items.AddChild(Node,'vOutro='  +FloatToStr(Total.ICMSTot.vOutro));
       trvwCTe.Items.AddChild(Node,'vNF='     +FloatToStr(Total.ICMSTot.vNF));

       if Total.ISSQNtot.vServ > 0 then
        begin
          Node := trvwCTe.Items.AddChild(NodePai,'ISSQNtot');
          trvwCTe.Items.AddChild(Node,'vServ='   +FloatToStr(Total.ISSQNtot.vServ));
          trvwCTe.Items.AddChild(Node,'vBC='     +FloatToStr(Total.ISSQNTot.vBC));
          trvwCTe.Items.AddChild(Node,'vISS='    +FloatToStr(Total.ISSQNTot.vISS));
          trvwCTe.Items.AddChild(Node,'vPIS='    +FloatToStr(Total.ISSQNTot.vPIS));
          trvwCTe.Items.AddChild(Node,'vCOFINS=' +FloatToStr(Total.ISSQNTot.vCOFINS));
        end;

       Node := trvwCTe.Items.AddChild(NodePai,'retTrib');
       trvwCTe.Items.AddChild(Node,'vRetPIS='   +FloatToStr(Total.retTrib.vRetPIS));
       trvwCTe.Items.AddChild(Node,'vRetCOFINS='+FloatToStr(Total.retTrib.vRetCOFINS));
       trvwCTe.Items.AddChild(Node,'vRetCSLL='  +FloatToStr(Total.retTrib.vRetCSLL));
       trvwCTe.Items.AddChild(Node,'vBCIRRF='   +FloatToStr(Total.retTrib.vBCIRRF));
       trvwCTe.Items.AddChild(Node,'vIRRF='     +FloatToStr(Total.retTrib.vIRRF));
       trvwCTe.Items.AddChild(Node,'vBCRetPrev='+FloatToStr(Total.retTrib.vBCRetPrev));
       trvwCTe.Items.AddChild(Node,'vRetPrev='  +FloatToStr(Total.retTrib.vRetPrev));

       NodePai := trvwCTe.Items.AddChild(Nota,'Transp');
       Node := trvwCTe.Items.AddChild(NodePai,'Transporta');
       trvwCTe.Items.AddChild(Node,'modFrete=' +modFreteToStr(Transp.modFrete));
       trvwCTe.Items.AddChild(Node,'CNPJCPF='  +Transp.Transporta.CNPJCPF);
       trvwCTe.Items.AddChild(Node,'xNome='    +Transp.Transporta.xNome);
       trvwCTe.Items.AddChild(Node,'IE='       +Transp.Transporta.IE);
       trvwCTe.Items.AddChild(Node,'xEnder='   +Transp.Transporta.xEnder);
       trvwCTe.Items.AddChild(Node,'xMun='     +Transp.Transporta.xMun);
       trvwCTe.Items.AddChild(Node,'UF='       +Transp.Transporta.UF);

       Node := trvwCTe.Items.AddChild(NodePai,'retTransp');
       trvwCTe.Items.AddChild(Node,'vServ='    +FloatToStr(Transp.retTransp.vServ));
       trvwCTe.Items.AddChild(Node,'vBCRet='   +FloatToStr(Transp.retTransp.vBCRet));
       trvwCTe.Items.AddChild(Node,'pICMSRet=' +FloatToStr(Transp.retTransp.pICMSRet));
       trvwCTe.Items.AddChild(Node,'vICMSRet=' +FloatToStr(Transp.retTransp.vICMSRet));
       trvwCTe.Items.AddChild(Node,'CFOP='     +Transp.retTransp.CFOP);
       trvwCTe.Items.AddChild(Node,'cMunFG='   +FloatToStr(Transp.retTransp.cMunFG));

       Node := trvwCTe.Items.AddChild(NodePai,'veicTransp');
       trvwCTe.Items.AddChild(Node,'placa='  +Transp.veicTransp.placa);
       trvwCTe.Items.AddChild(Node,'UF='     +Transp.veicTransp.UF);
       trvwCTe.Items.AddChild(Node,'RNTC='   +Transp.veicTransp.RNTC);

       for I:=0 to Transp.Reboque.Count-1 do
        begin
          Node := trvwCTe.Items.AddChild(NodePai,'Reboque'+IntToStrZero(I+1,3));
          with Transp.Reboque.Items[I] do
           begin
             trvwCTe.Items.AddChild(Node,'placa=' +placa);
             trvwCTe.Items.AddChild(Node,'UF='    +UF);
             trvwCTe.Items.AddChild(Node,'RNTC='  +RNTC);
           end;
        end;

       for I:=0 to Transp.Vol.Count-1 do
        begin
          Node := trvwCTe.Items.AddChild(NodePai,'Volume'+IntToStrZero(I+1,3));
          with Transp.Vol.Items[I] do
           begin
             trvwCTe.Items.AddChild(Node,'qVol='  +IntToStr(qVol));
             trvwCTe.Items.AddChild(Node,'esp='   +esp);
             trvwCTe.Items.AddChild(Node,'marca=' +marca);
             trvwCTe.Items.AddChild(Node,'nVol='  +nVol);
             trvwCTe.Items.AddChild(Node,'pesoL=' +FloatToStr(pesoL));
             trvwCTe.Items.AddChild(Node,'pesoB'  +FloatToStr(pesoB));

             for J:=0 to Lacres.Count-1 do
              begin
                Node := trvwCTe.Items.AddChild(Node,'Lacre'+IntToStrZero(I+1,3)+IntToStrZero(J+1,3) );
                trvwCTe.Items.AddChild(Node,'nLacre='+Lacres.Items[J].nLacre);
              end;
           end;
        end;

       NodePai := trvwCTe.Items.AddChild(Nota,'Cobr');
       Node    := trvwCTe.Items.AddChild(NodePai,'Fat');
       trvwCTe.Items.AddChild(Node,'nFat='  +Cobr.Fat.nFat);
       trvwCTe.Items.AddChild(Node,'vOrig=' +FloatToStr(Cobr.Fat.vOrig));
       trvwCTe.Items.AddChild(Node,'vDesc=' +FloatToStr(Cobr.Fat.vDesc));
       trvwCTe.Items.AddChild(Node,'vLiq='  +FloatToStr(Cobr.Fat.vLiq));

       for I:=0 to Cobr.Dup.Count-1 do
        begin
          Node    := trvwCTe.Items.AddChild(NodePai,'Duplicata'+IntToStrZero(I+1,3));
          with Cobr.Dup.Items[I] do
           begin
             trvwCTe.Items.AddChild(Node,'nDup='  +nDup);
             trvwCTe.Items.AddChild(Node,'dVenc=' +DateToStr(dVenc));
             trvwCTe.Items.AddChild(Node,'vDup='  +FloatToStr(vDup));
           end;
        end;

       NodePai := trvwCTe.Items.AddChild(Nota,'InfAdic');
       trvwCTe.Items.AddChild(NodePai,'infCpl='     +InfAdic.infCpl);
       trvwCTe.Items.AddChild(NodePai,'infAdFisco=' +InfAdic.infAdFisco);

       for I:=0 to InfAdic.obsCont.Count-1 do
        begin
          Node := trvwCTe.Items.AddChild(NodePai,'obsCont'+IntToStrZero(I+1,3));
          with InfAdic.obsCont.Items[I] do
           begin
             trvwCTe.Items.AddChild(Node,'xCampo=' +xCampo);
             trvwCTe.Items.AddChild(Node,'xTexto=' +xTexto);
           end;
        end;

         for I:=0 to InfAdic.obsFisco.Count-1 do
          begin
            Node := trvwCTe.Items.AddChild(NodePai,'obsFisco'+IntToStrZero(I+1,3));
            with InfAdic.obsFisco.Items[I] do
             begin
                trvwCTe.Items.AddChild(Node,'xCampo=' +xCampo);
                trvwCTe.Items.AddChild(Node,'xTexto=' +xTexto);
             end;
          end;

         for I:=0 to InfAdic.procRef.Count-1 do
          begin
            Node := trvwCTe.Items.AddChild(NodePai,'procRef'+IntToStrZero(I+1,3));
            with InfAdic.procRef.Items[I] do
             begin
               trvwCTe.Items.AddChild(Node,'nProc='   +nProc);
               trvwCTe.Items.AddChild(Node,'indProc=' +indProcToStr(indProc));
             end;
          end;

         if (exporta.UFembarq <> '') then
          begin
            Node := trvwCTe.Items.AddChild(Nota,'exporta');
            trvwCTe.Items.AddChild(Node,'UFembarq='   +exporta.UFembarq);
            trvwCTe.Items.AddChild(Node,'xLocEmbarq=' +exporta.xLocEmbarq);
          end;

         if (compra.xNEmp <> '') then
          begin
            Node := trvwCTe.Items.AddChild(Nota,'compra');
            trvwCTe.Items.AddChild(Node,'xNEmp=' +compra.xNEmp);
            trvwCTe.Items.AddChild(Node,'xPed='  +compra.xPed);
            trvwCTe.Items.AddChild(Node,'xCont=' +compra.xCont);
          end;
      *)
      end;
     PageControl2.ActivePageIndex := 3;
    end;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnValidarXMLClick(Sender: TObject);
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
   showmessage('Conhecimento de Transporte Eletrônico Valido');
  end;
end;

procedure TfrmDemo_ACBrCTe.btnCancCTeClick(Sender: TObject);
var
 vAux : String;
begin
 OpenDialog1.Title := 'Selecione o CTe';
 OpenDialog1.DefaultExt := '*-cte.xml';
 OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

 if OpenDialog1.Execute then
  begin
   ACBrCTe1.Conhecimentos.Clear;
   ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
   if not(InputQuery('WebServices Cancelamento', 'Justificativa', vAux))
    then exit;

   ACBrCTe1.Cancelamento(vAux);
   MemoResp.Lines.Text :=  UTF8Encode(ACBrCTe1.WebServices.EnvEvento.RetWS);
   memoRespWS.Lines.Text :=  UTF8Encode(ACBrCTe1.WebServices.EnvEvento.RetWS);
   LoadXML(MemoResp, WBResposta);
   ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
   ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmDemo_ACBrCTe.btnCancelarChaveClick(Sender: TObject);
begin
 ShowMessage('Opção não Implementada!');
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
   ACBrCTe1.Conhecimentos.Imprimir;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnGerarPDFClick(Sender: TObject);
begin
 OpenDialog1.Title := 'Selecione o CTe';
 OpenDialog1.DefaultExt := '*-cte.xml';
 OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

 if OpenDialog1.Execute
  then begin
   ACBrCTe1.Conhecimentos.Clear;
   ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
   ACBrCTe1.Conhecimentos.ImprimirPDF;
  end;
end;

procedure TfrmDemo_ACBrCTe.btnEnviarEmailClick(Sender: TObject);
var
 Para : String;
 CC   : Tstrings;
begin
 if not(InputQuery('Enviar Email', 'Email de destino', Para))
  then exit;

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
                                              ); //Pede confirmação de leitura do email
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
 Para : String;
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
    ACBrCTe1.EnviarEmailEvento(
                              Para
                             , edtEmailAssunto.Text
                             , mmEmailMsg.Lines
                             ); // Auto TLS
    CC.Free;
    Evento.Free;
  end;
end;

end.
