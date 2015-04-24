{$I ACBr.inc}

unit Frm_Demo_ACBrNFSe;

interface

uses IniFiles, LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, Buttons,
  ExtCtrls, IpHtml, pcnConversao, pnfsConversao, ACBrNFSe, ACBrNFSeDANFSeClass,
  pnfsNFSe, ACBrNFSeDANFSeRLClass;

type

  { TfrmDemo_ACBrNFSe }

  TfrmDemo_ACBrNFSe = class(TForm)
    ACBrNFSeDANFSeRL1: TACBrNFSeDANFSeRL;
    WBResposta: TIpHtmlPanel;
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    gbConfiguracoes: TGroupBox;
    PageControl1: TPageControl;
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
    ckVisualizar: TCheckBox;
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
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
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
    procedure sbtnCaminhoCertClick(Sender: TObject);
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
    procedure ACBrNFSe1GerarLog(const Mensagem: String);
    procedure sbtSchemasClick(Sender: TObject);
    procedure sbtnPrestLogoClick(Sender: TObject);
    procedure btnConsultarNFSeRPSClick(Sender: TObject);
    procedure btnConsultarNFSePeriodoClick(Sender: TObject);
    procedure cbCidadesChange(Sender: TObject);
    function RoundTo5(Valor: Double; Casas: Integer): Double;
    procedure btnGerarEnviarNFSeClick(Sender: TObject);
    procedure btnEnviaremailClick(Sender: TObject);
    procedure btnLinkNFSeClick(Sender: TObject);
    procedure btnGerarLoteRPSClick(Sender: TObject);
    procedure btnGerarEnviarSincronoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    {
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    }
  private
    { Private declarations }
    Ok : Boolean;
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfiguraComponente;
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TIpHtmlPanel);
    procedure GerarNFSe(NumNFSe : String);
  public
    { Public declarations }
  end;

var
  frmDemo_ACBrNFSe: TfrmDemo_ACBrNFSe;

implementation

uses
 FileCtrl, DateUtils, Math,
 ufrmStatus,
 ACBrNFSeNotasFiscais, ACBrDFeUtil, ACBrNFSeUtil;

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

procedure TfrmDemo_ACBrNFSe.GravarConfiguracao;
var
 IniFile    : String;
 Ini        : TIniFile;
 StreamMemo : TMemoryStream;
begin
 IniFile := ChangeFileExt( Application.ExeName, '.ini');

 Ini := TIniFile.Create( IniFile );
 try
  Ini.WriteString( 'Emitente', 'CNPJ'       , edtEmitCNPJ.Text);
  Ini.WriteString( 'Emitente', 'IM'         , edtEmitIM.Text);
  Ini.WriteString( 'Emitente', 'RazaoSocial', edtEmitRazao.Text);
  Ini.WriteString( 'Emitente', 'Fantasia'   , edtEmitFantasia.Text);
  Ini.WriteString( 'Emitente', 'Fone'       , edtEmitFone.Text);
  Ini.WriteString( 'Emitente', 'CEP'        , edtEmitCEP.Text);
  Ini.WriteString( 'Emitente', 'Logradouro' , edtEmitLogradouro.Text);
  Ini.WriteString( 'Emitente', 'Numero'     , edtEmitNumero.Text);
  Ini.WriteString( 'Emitente', 'Complemento', edtEmitComp.Text);
  Ini.WriteString( 'Emitente', 'Bairro'     , edtEmitBairro.Text);
  Ini.WriteString( 'Emitente', 'CodCidade'  , edtCodCidade.Text);
  Ini.WriteString( 'Emitente', 'Cidade'     , edtEmitCidade.Text);
  Ini.WriteString( 'Emitente', 'UF'         , edtEmitUF.Text);

  Ini.WriteString( 'Certificado', 'Caminho'    , edtCaminho.Text);
  Ini.WriteString( 'Certificado', 'Senha'      , edtSenha.Text);
  Ini.WriteString( 'Certificado', 'NumSerie'   , edtNumSerie.Text);

  Ini.WriteString( 'Geral', 'Schemas'   , edtSchemas.Text);
  Ini.WriteString( 'Geral', 'LogoMarca' , edtLogoMarca.Text);
  Ini.WriteString( 'Geral', 'PrestLogo' , edtPrestLogo.Text);
  Ini.WriteBool(   'Geral', 'Salvar'    , ckSalvar.Checked);
  Ini.WriteString( 'Geral', 'PathSalvar', edtPathLogs.Text);
  Ini.WriteString( 'Geral', 'Prefeitura', edtPrefeitura.Text);

  Ini.WriteInteger( 'WebService', 'Ambiente'  , rgTipoAmb.ItemIndex);
  Ini.WriteBool(    'WebService', 'Visualizar', ckVisualizar.Checked);
  Ini.WriteString(  'WebService', 'SenhaWeb'  , edtSenhaWeb.Text);
  Ini.WriteString(  'WebService', 'UserWeb'   , edtUserWeb.Text);

  Ini.WriteString( 'Proxy', 'Host' , edtProxyHost.Text);
  Ini.WriteString( 'Proxy', 'Porta', edtProxyPorta.Text);
  Ini.WriteString( 'Proxy', 'User' , edtProxyUser.Text);
  Ini.WriteString( 'Proxy', 'Pass' , edtProxySenha.Text);

  Ini.WriteString( 'Email', 'Host'     , edtSmtpHost.Text);
  Ini.WriteString( 'Email', 'Port'     , edtSmtpPort.Text);
  Ini.WriteString( 'Email', 'User'     , edtSmtpUser.Text);
  Ini.WriteString( 'Email', 'Pass'     , edtSmtpPass.Text);
  Ini.WriteString( 'Email', 'Assunto'  , edtEmailAssunto.Text);
  Ini.WriteBool(   'Email', 'SSL'      , cbEmailSSL.Checked );
  Ini.WriteString( 'Email', 'Remetente', edtEmailRemetente.Text);

  StreamMemo := TMemoryStream.Create;
  mmEmailMsg.Lines.SaveToStream(StreamMemo);
  StreamMemo.Seek(0, soFromBeginning);
  Ini.WriteBinaryStream( 'Email', 'Mensagem', StreamMemo);

  StreamMemo.Free;
 finally
  Ini.Free;
 end;
end;

procedure TfrmDemo_ACBrNFSe.LerConfiguracao;
var
 IniFile    : String;
 Ini        : TIniFile;
 StreamMemo : TMemoryStream;
begin
 IniFile := ChangeFileExt( Application.ExeName, '.ini');

 Ini := TIniFile.Create( IniFile );
 try
  {$IFDEF ACBrNFSeOpenSSL}
   edtCaminho.Text  := Ini.ReadString( 'Certificado', 'Caminho' , '');
   edtSenha.Text    := Ini.ReadString( 'Certificado', 'Senha'   , '');
   edtNumSerie.Visible := False;
   Label25.Visible     := False;
   sbtnGetCert.Visible := False;
  {$ELSE}
   edtNumSerie.Text := Ini.ReadString( 'Certificado', 'NumSerie', '');
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

  edtEmitCNPJ.Text       := Ini.ReadString( 'Emitente', 'CNPJ'       , '');
  edtEmitIM.Text         := Ini.ReadString( 'Emitente', 'IM'         , '');
  edtEmitRazao.Text      := Ini.ReadString( 'Emitente', 'RazaoSocial', '');
  edtEmitFantasia.Text   := Ini.ReadString( 'Emitente', 'Fantasia'   , '');
  edtEmitFone.Text       := Ini.ReadString( 'Emitente', 'Fone'       , '');
  edtEmitCEP.Text        := Ini.ReadString( 'Emitente', 'CEP'        , '');
  edtEmitLogradouro.Text := Ini.ReadString( 'Emitente', 'Logradouro' , '');
  edtEmitNumero.Text     := Ini.ReadString( 'Emitente', 'Numero'     , '');
  edtEmitComp.Text       := Ini.ReadString( 'Emitente', 'Complemento', '');
  edtEmitBairro.Text     := Ini.ReadString( 'Emitente', 'Bairro'     , '');
  edtEmitCidade.Text     := Ini.ReadString( 'Emitente', 'Cidade'     , '');
  edtEmitUF.Text         := Ini.ReadString( 'Emitente', 'UF'         , '');
  edtCodCidade.Text      := Ini.ReadString( 'Emitente', 'CodCidade'  , '');
  cbCidades.ItemIndex    := cbCidades.Items.IndexOf(edtEmitCidade.Text + '/' +
                                                    edtCodCidade.Text + '/' +
                                                    edtEmitUF.Text);

  edtSchemas.Text       := Ini.ReadString( 'Geral', 'Schemas'   , '');
  edtLogoMarca.Text     := Ini.ReadString( 'Geral', 'LogoMarca' , '');
  edtPrestLogo.Text     := Ini.ReadString( 'Geral', 'PrestLogo' , '');
  ckSalvar.Checked      := Ini.ReadBool(   'Geral', 'Salvar'    , True);
  edtPathLogs.Text      := Ini.ReadString( 'Geral', 'PathSalvar', '');
  edtPrefeitura.Text    := Ini.ReadString( 'Geral', 'Prefeitura', '');

  rgTipoAmb.ItemIndex  := Ini.ReadInteger( 'WebService', 'Ambiente'  , 0);
  ckVisualizar.Checked := Ini.ReadBool(    'WebService', 'Visualizar', False);
  edtSenhaWeb.Text     := Ini.ReadString(  'WebService', 'SenhaWeb'  , '');
  edtUserWeb.Text      := Ini.ReadString(  'WebService', 'UserWeb'  , '');

  edtProxyHost.Text  := Ini.ReadString( 'Proxy', 'Host' , '');
  edtProxyPorta.Text := Ini.ReadString( 'Proxy', 'Porta', '');
  edtProxyUser.Text  := Ini.ReadString( 'Proxy', 'User' , '');
  edtProxySenha.Text := Ini.ReadString( 'Proxy', 'Pass' , '');

  edtSmtpHost.Text       := Ini.ReadString( 'Email', 'Host'   , '');
  edtSmtpPort.Text       := Ini.ReadString( 'Email', 'Port'   , '');
  edtSmtpUser.Text       := Ini.ReadString( 'Email', 'User'   , '');
  edtSmtpPass.Text       := Ini.ReadString( 'Email', 'Pass'   , '');
  edtEmailAssunto.Text   := Ini.ReadString( 'Email', 'Assunto', '');
  cbEmailSSL.Checked     := Ini.ReadBool(   'Email', 'SSL'    , False);
  edtEmailRemetente.Text := Ini.ReadString( 'Email', 'Remetente', '');

  StreamMemo := TMemoryStream.Create;
  Ini.ReadBinaryStream( 'Email', 'Mensagem', StreamMemo);
  mmEmailMsg.Lines.LoadFromStream(StreamMemo);
  StreamMemo.Free;
 finally
  Ini.Free;
 end;
end;

procedure TfrmDemo_ACBrNFSe.ConfiguraComponente;
var
 PathMensal: String;
begin
 {$IFDEF ACBrNFSeOpenSSL}
   ACBrNFSe1.Configuracoes.Certificados.Certificado := edtCaminho.Text;
   ACBrNFSe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
 {$ELSE}
   ACBrNFSe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;
 {$ENDIF}

 ACBrNFSe1.Configuracoes.Arquivos.AdicionarLiteral:=True;
 ACBrNFSe1.Configuracoes.Arquivos.EmissaoPathNFSe:=True;
 ACBrNFSe1.Configuracoes.Arquivos.PastaMensal:=True;
 ACBrNFSe1.Configuracoes.Arquivos.PathCan:=edtPathLogs.Text;
 ACBrNFSe1.Configuracoes.Arquivos.PathNFSe:=edtPathLogs.Text;
 ACBrNFSe1.Configuracoes.Arquivos.Salvar:=True;

 PathMensal:=ACBrNFSe1.Configuracoes.Arquivos.GetPathNFSe(0);

 ACBrNFSe1.Configuracoes.Geral.PathSchemas := edtSchemas.Text;
 ACBrNFSe1.Configuracoes.Geral.Salvar      := ckSalvar.Checked;
 ACBrNFSe1.Configuracoes.Geral.PathSalvar  := edtPathLogs.Text;

 ACBrNFSe1.Configuracoes.WebServices.CodigoMunicipio := StrToIntDef(edtCodCidade.Text, 0);
 ACBrNFSe1.Configuracoes.WebServices.Ambiente        := StrToTpAmb(Ok, IntToStr(rgTipoAmb.ItemIndex+1));
 ACBrNFSe1.Configuracoes.WebServices.Visualizar      := ckVisualizar.Checked;
 ACBrNFSe1.Configuracoes.WebServices.SenhaWeb        := edtSenhaWeb.Text;
 ACBrNFSe1.Configuracoes.WebServices.UserWeb         := edtUserWeb.Text;

 ACBrNFSe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
 ACBrNFSe1.Configuracoes.WebServices.ProxyPort := edtProxyPorta.Text;
 ACBrNFSe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
 ACBrNFSe1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;

 ACBrNFSe1.Configuracoes.WebServices.SetConfigMunicipio(ACBrNFSe1.Configuracoes.Geral.PathSchemas);

 if ACBrNFSe1.DANFSe <> nil then
  begin
   ACBrNFSe1.DANFSe.Logo       := edtLogoMarca.Text;
   ACBrNFSe1.DANFSe.PrestLogo  := edtPrestLogo.Text;
   ACBrNFSe1.DANFSe.Prefeitura := edtPrefeitura.Text;
  end;

 lblSchemas.Caption := ACBrNFSe1.Configuracoes.WebServices.xProvedor;
end;

procedure TfrmDemo_ACBrNFSe.LoadXML(MyMemo: TMemo;  MyWebBrowser: TIpHtmlPanel);
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

procedure TfrmDemo_ACBrNFSe.GerarNFSe(NumNFSe: String);
var
 ValorISS: Double;
begin
 ACBrNFSe1.NotasFiscais.Clear;

 with ACBrNFSe1 do
  begin
   NotasFiscais.NumeroLote:='1';

   with NotasFiscais.Add.NFSe do
    begin
     IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumNFSe));

     // Para o provedor ISS.NET em ambiente de Homologação mudar a série para '8'
     IdentificacaoRps.Serie := 'UNICA';

     // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
     IdentificacaoRps.Tipo := trRPS;

     DataEmissao := Date;

     // TnfseNaturezaOperacao = ( noTributacaoNoMunicipio, noTributacaoForaMunicipio, noIsencao, noImune, noSuspensaDecisaoJudicial, noSuspensaProcedimentoAdministrativo );
     NaturezaOperacao := noTributacaoNoMunicipio;
//     NaturezaOperacao := noTributacaoNoMunicipio51; 

     // TnfseRegimeEspecialTributacao = ( retNenhum, retMicroempresaMunicipal, retEstimativa, retSociedadeProfissionais, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP );
//     RegimeEspecialTributacao := retNenhum;
     RegimeEspecialTributacao := retMicroempresaMunicipal;

     // TnfseSimNao = ( snSim, snNao );
     OptanteSimplesNacional := snSim;

     // TnfseSimNao = ( snSim, snNao );
     IncentivadorCultural := snSim;

     // TnfseSimNao = ( snSim, snNao );
     // snSim = Ambiente de Produção
     // snNao = Ambiente de Homologação
     Producao := snNao;

     // TnfseStatusRPS = ( srNormal, srCancelado );
     Status := srNormal;

     // Somente Os provedores Betha, FISSLex e SimplISS permitem incluir no RPS
     // a TAG: OutrasInformacoes os demais essa TAG é gerada e preenchida pelo
     // WebService do provedor.
     OutrasInformacoes := 'Pagamento a Vista';

     (* Usando quando o RPS for substituir outro
     RpsSubstituido.Numero := FormatFloat('#########0', i);
     RpsSubstituido.Serie  := 'UNICA';
     // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
     RpsSubstituido.Tipo   := trRPS;
     *)

     Servico.Valores.ValorServicos          := 1685.50;
     Servico.Valores.ValorDeducoes          := 0.00;
     Servico.Valores.ValorPis               := 0.00;
     Servico.Valores.ValorCofins            := 0.00;
     Servico.Valores.ValorInss              := 0.00;
     Servico.Valores.ValorIr                := 0.00;
     Servico.Valores.ValorCsll              := 0.00;

     // TnfseSituacaoTributaria = ( stRetencao, stNormal, stSubstituicao );
     // stRetencao = snSim
     // stNormal   = snNao
     Servico.Valores.IssRetido              := stNormal;

     Servico.Valores.OutrasRetencoes        := 0.00;
     Servico.Valores.DescontoIncondicionado := 0.00;
     Servico.Valores.DescontoCondicionado   := 0.00;

     Servico.Valores.BaseCalculo            := Servico.Valores.ValorServicos -
                                               Servico.Valores.ValorDeducoes -
                                               Servico.Valores.DescontoIncondicionado;
     Servico.Valores.Aliquota               := 0.03;

     if Servico.Valores.IssRetido = stNormal
      then begin
       ValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota;

       // A função RoundTo5 é usada para arredondar valores, sendo que o segundo
       // parametro se refere ao numero de casas decimais.
       // exemplos: RoundTo5(50.532, -2) ==> 50.53
       // exemplos: RoundTo5(50.535, -2) ==> 50.54
       // exemplos: RoundTo5(50.536, -2) ==> 50.54

       Servico.Valores.ValorIss       := RoundTo5(ValorISS, -2);
       Servico.Valores.ValorIssRetido := 0.00;
      end
      else begin
       ValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota;

       Servico.Valores.ValorIss       := 0.00;
       Servico.Valores.ValorIssRetido := RoundTo5(ValorISS, -2);
      end;

     Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
                                         Servico.Valores.ValorPis -
                                         Servico.Valores.ValorCofins -
                                         Servico.Valores.ValorInss -
                                         Servico.Valores.ValorIr -
                                         Servico.Valores.ValorCsll -
                                         Servico.Valores.OutrasRetencoes -
                                         Servico.Valores.ValorIssRetido -
                                         Servico.Valores.DescontoIncondicionado -
                                         Servico.Valores.DescontoCondicionado;

     Servico.ItemListaServico         := '01.07';

     // Para o provedor ISS.NET em ambiente de Homologação
     // o Codigo CNAE tem que ser '6511102'
     // Servico.CodigoCnae                := '123'; // Informação Opcional
     Servico.CodigoTributacaoMunicipio := '118879';
     Servico.Discriminacao             := 'discriminacao';

     // Para o provedor ISS.NET em ambiente de Homologação
     // o Codigo do Municipio tem que ser '999'
     Servico.CodigoMunicipio := edtCodCidade.Text;

     // Informar A Exigibilidade ISS para fintelISS [1/2/3/4/5/6/7]
     Servico.ExigibilidadeISS := exiExigivel;
     
     // Informar para Saatri
     Servico.CodigoPais := 1058; // Brasil
     Servico.MunicipioIncidencia := StrToIntDef(edtCodCidade.Text, 0);
     
    // Somente o provedor SimplISS permite infomar mais de 1 serviço
     with Servico.ItemServico.Add do
      begin
       Descricao     := 'SERVICO 1';
       Quantidade    := 1;
       ValorUnitario := 15.00;
      end;

     Prestador.Cnpj               := edtEmitCNPJ.Text;
     Prestador.InscricaoMunicipal := edtEmitIM.Text;

     // Para o provedor ISSDigital deve-se informar também:
     Prestador.Senha        := 'senha';
     Prestador.FraseSecreta := 'frase secreta';
     Prestador.cUF          := 33;

     Tomador.IdentificacaoTomador.CpfCnpj            := '99999999000191';
     Tomador.IdentificacaoTomador.InscricaoMunicipal := '1733160024';

     Tomador.RazaoSocial := 'INSCRICAO DE TESTE';

     Tomador.Endereco.Endereco        := 'RUA PRINCIPAL';
     Tomador.Endereco.Numero          := '100';
     Tomador.Endereco.Complemento     := 'APTO 11';
     Tomador.Endereco.Bairro          := 'CENTRO';
     Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
     Tomador.Endereco.UF              := edtEmitUF.Text;
     Tomador.Endereco.CEP             := edtEmitCEP.Text;
	 //Provedor Equiplano é obrigatório o pais e IE
     Tomador.Endereco.xPais           := 'BRASIL';	 
     Tomador.IdentificacaoTomador.InscricaoEstadual := '123456';

     Tomador.Contato.Telefone := '1122223333';
     Tomador.Contato.Email    := 'nome@provedor.com.br';

     (* Usando quando houver um intermediario na prestação do serviço
     IntermediarioServico.RazaoSocial        := 'razao';
     IntermediarioServico.CpfCnpj            := '00000000000';
     IntermediarioServico.InscricaoMunicipal := '12547478';
     *)

     (* Usando quando o serviço for uma obra
     ConstrucaoCivil.CodigoObra := '88888';
     ConstrucaoCivil.Art        := '433';
     *)
    end;

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

procedure TfrmDemo_ACBrNFSe.sbtnGetCertClick(Sender: TObject);
begin
 {$IFNDEF ACBrNFSeOpenSSL}
  edtNumSerie.Text := ACBrNFSe1.Configuracoes.Certificados.SelecionarCertificado;
 {$ENDIF}
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
 Dir : string;
begin
 if Length(edtPathLogs.Text) <= 0
  then Dir := ExtractFileDir(application.ExeName)
  else Dir := edtPathLogs.Text;

 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP)
  then edtPathLogs.Text := Dir;
end;

procedure TfrmDemo_ACBrNFSe.FormCreate(Sender: TObject);
begin
 PageControl1.ActivePageIndex:=0;

 LerConfiguracao;
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
 OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5'); { *Converted from ShellExecute* }
end;

procedure TfrmDemo_ACBrNFSe.lblPatrocinadorClick(Sender: TObject);
begin
 OpenURL('http://acbr.sourceforge.net/drupal/?q=node/35'); { *Converted from ShellExecute* }
end;

procedure TfrmDemo_ACBrNFSe.lblDoar1Click(Sender: TObject);
begin
 OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14'); { *Converted from ShellExecute* }
end;

procedure TfrmDemo_ACBrNFSe.btnGerarEnviarLoteClick(Sender: TObject);
var
 vAux, vNumLote : String;
begin
 if not(InputQuery('Gerar e Enviar Lote', 'Numero do RPS', vAux))
  then exit;

 if not(InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote))
  then exit;

 ACBrNFSe1.NotasFiscais.Clear;
 GerarNFSe(vAux);
 ACBrNFSe1.Enviar(vNumLote);

 ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmDemo_ACBrNFSe.btnConsultarLoteClick(Sender: TObject);
var
 Lote, Protocolo : String;
begin
 if not(InputQuery('Consultar Lote', 'Número do Lote', Lote))
  then exit;
 if not(InputQuery('Consultar Lote', 'Número do Protocolo', Protocolo))
  then exit;

 ACBrNFSe1.ConsultarLoteRps(Lote, Protocolo);

 MemoResp.Lines.Text   := UTF8Encode(ACBrNFSe1.WebServices.ConsLote.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsLote.RetWS);
 LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrNFSe.btnCancNFSeClick(Sender: TObject);
var
 Codigo, Motivo : String; 
begin

 OpenDialog1.Title := 'Selecione a NFSe';
 OpenDialog1.DefaultExt := '*-NFSe.xml';
 OpenDialog1.Filter := 'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Geral.PathSalvar;

 if OpenDialog1.Execute then
  begin
   ACBrNFSe1.NotasFiscais.Clear;
   ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

   // Codigo de Cancelamento
   // 1 - Erro de emissão
   // 2 - Serviço não concluido
   // 3 - RPS Cancelado na Emissão

   if not(InputQuery('Cancelar NFSe', 'Código de Cancelamento', Codigo))
    then exit;
	
   //Provedor Equiplano é obrigatório o motivo de cancelamento
   //if not(InputQuery('Cancelar NFSe', 'Motivo de Cancelamento', Motivo))
   // then exit;
   //ACBrNFSe1.NotasFiscais.Items[0].NFSe.MotivoCancelamento:= Motivo;

//   ACBrNFSe1.WebServices.CancelaNFSe(Codigo, '1', '03310700000170', '0306223', '0');
   ACBrNFSe1.CancelarNFSe(Codigo);

   MemoDados.Lines.Add('Arquivo Carregado de: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
   MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
   MemoDados.Lines.Add('Retorno do Cancelamento:');
   MemoDados.Lines.Add('Cód. Cancelamento: ' + ACBrNFSe1.WebServices.CancNfse.CodigoCancelamento);
   MemoDados.Lines.Add('Data / Hora      : ' +
    DFeUtil.SeSenao(ACBrNFSe1.WebServices.CancNfse.DataHora = 0, '',
                      DateTimeToStr(ACBrNFSe1.WebServices.CancNfse.DataHora)));
   LoadXML(MemoResp, WBResposta);
   PageControl2.ActivePageIndex := 1;
  end;

end;

procedure TfrmDemo_ACBrNFSe.btnConsultarSitLoteClick(Sender: TObject);
var
 Protocolo : String;
begin
 if not(InputQuery('Consultar Situação do Lote', 'Número do Protocolo', Protocolo))
  then exit;

 ACBrNFSe1.ConsultarSituacao(edtEmitCNPJ.Text, edtEmitIM.Text, Protocolo);

 MemoResp.Lines.Text   := UTF8Encode(ACBrNFSe1.WebServices.ConsSitLote.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsSitLote.RetWS);
 LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrNFSe.btnGerarRPSClick(Sender: TObject);
var
 vAux : String;
begin
 if not(InputQuery('Gerar RPS', 'Numero do RPS', vAux))
  then exit;

 ACBrNFSe1.NotasFiscais.Clear;
 GerarNFSe(vAux);
 ACBrNFSe1.NotasFiscais.Items[0].SaveToFile;

 ShowMessage('Arquivo gerado em: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
 MemoDados.Lines.Add('Arquivo gerado em: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
 MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
 LoadXML(MemoResp, WBResposta);
 PageControl2.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrNFSe.btnImprimirClick(Sender: TObject);
begin
 OpenDialog1.Title := 'Selecione a NFSe';
 OpenDialog1.DefaultExt := '*-NFSe.xml';
 OpenDialog1.Filter := 'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Geral.PathSalvar;

 if OpenDialog1.Execute then
  begin
   ACBrNFSe1.NotasFiscais.Clear;
   ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
   ACBrNFSe1.NotasFiscais.Imprimir;

   MemoDados.Lines.Add('Arquivo Carregado de: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
   MemoDados.Lines.Add('Nota Numero: '+ACBrNFSe1.NotasFiscais.Items[0].NFSe.Numero);
   MemoDados.Lines.Add('Código de Verificação: '+ACBrNFSe1.NotasFiscais.Items[0].NFSe.CodigoVerificacao);
   MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
   LoadXML(MemoResp, WBResposta);
   PageControl2.ActivePageIndex := 1;
  end;
end;

procedure TfrmDemo_ACBrNFSe.ACBrNFSe1StatusChange(Sender: TObject);
begin
 case ACBrNFSe1.Status of
  stNFSeIdle : begin
            if ( frmStatus <> nil ) then frmStatus.Hide;
           end;
  stNFSeRecepcao : begin
                   if ( frmStatus = nil )
                    then frmStatus := TfrmStatus.Create(Application);
                   frmStatus.lblStatus.Caption := 'Enviando dados da NFSe...';
                   frmStatus.Show;
                   frmStatus.BringToFront;
                  end;
  stNFSeConsulta : begin
                   if ( frmStatus = nil )
                    then frmStatus := TfrmStatus.Create(Application);
                   frmStatus.lblStatus.Caption := 'Consultando...';
                   frmStatus.Show;
                   frmStatus.BringToFront;
                  end;
  stNFSeCancelamento : begin
                       if ( frmStatus = nil )
                        then frmStatus := TfrmStatus.Create(Application);
                       frmStatus.lblStatus.Caption := 'Enviando cancelamento de NFSe...';
                       frmStatus.Show;
                       frmStatus.BringToFront;
                      end;
  stNFSeEmail : begin
                if ( frmStatus = nil )
                 then frmStatus := TfrmStatus.Create(Application);
                frmStatus.lblStatus.Caption := 'Enviando Email...';
                frmStatus.Show;
                frmStatus.BringToFront;
               end;
 end;
 Application.ProcessMessages;
end;

procedure TfrmDemo_ACBrNFSe.ACBrNFSe1GerarLog(const Mensagem: String);
begin
 memoLog.Lines.Add(Mensagem);
end;

procedure TfrmDemo_ACBrNFSe.sbtSchemasClick(Sender: TObject);
var
 Dir : string;
begin
 if Length(edtSchemas.Text) <= 0
  then Dir := ExtractFileDir(application.ExeName)
  else Dir := edtSchemas.Text;

 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP)
  then edtSchemas.Text := Dir;
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
 OpenDialog1.Filter := 'Arquivos Rps (*-Rps.xml)|*-Rps.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Geral.PathSalvar;

 if OpenDialog1.Execute then
  begin
   ACBrNFSe1.NotasFiscais.Clear;
   ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

   ACBrNFSe1.ConsultarNFSeporRps(ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero,
                                ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie,
                                TipoRPSToStr(ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo),
                                ACBrNFSe1.NotasFiscais.Items[0].NFSe.Prestador.Cnpj,
                                ACBrNFSe1.NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal);

   MemoResp.Lines.Text   := UTF8Encode(ACBrNFSe1.WebServices.ConsNfseRps.RetWS);
   memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsNfseRps.RetWS);
   LoadXML(MemoResp, WBResposta);
  end;
end;

procedure TfrmDemo_ACBrNFSe.btnConsultarNFSePeriodoClick(Sender: TObject);
var
 DataInicial, DataFinal : String;
begin
 if not(InputQuery('Consultar NFSe por Período', 'Data Inicial (DD/MM/AAAA):', DataInicial))
  then exit;
 if not(InputQuery('Consultar NFSe por Período', 'Data Final (DD/MM/AAAA):', DataFinal))
  then exit;

 ACBrNFSe1.ConsultarNFSe(edtEmitCNPJ.Text, edtEmitIM.Text, StrToDate(DataInicial), StrToDate(DataFinal));

 MemoResp.Lines.Text   := UTF8Encode(ACBrNFSe1.WebServices.ConsNfse.RetWS);
 memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsNfse.RetWS);
 LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrNFSe.cbCidadesChange(Sender: TObject);
var
 Tamanho: Integer;
begin
 Tamanho   := Length(Trim(cbCidades.Text));

 edtEmitCidade.Text := Copy(cbCidades.Text, 1, Tamanho - 11);
 edtEmitUF.Text     := Copy(cbCidades.Text, Tamanho - 1, 2);
 edtCodCidade.Text  := Copy(cbCidades.Text, Tamanho - 9, 7);
end;

// Função criada para arredondar valores quando a n Casa for maior ou igual a 5
// pois a função RoundTo arredonda quando a n Casa for maior ou igual a 6
function TfrmDemo_ACBrNFSe.RoundTo5(Valor: Double; Casas: Integer): Double;
var
 xValor, xDecimais: String;
 p, nCasas: Integer;
 nValor: Double;
begin
 nValor := Valor;
 xValor := Trim(FloatToStr(Valor));
 p      := pos(',', xValor);
 if Casas < 0
  then nCasas := - Casas
  else nCasas := Casas;
 if p > 0
  then begin
   xDecimais := Copy(xValor, p + 1, length(xValor));
   if length(xDecimais) > nCasas
    then begin
     if xDecimais[nCasas + 1] >= '5'
      then SetRoundMode(rmUP)
      else SetRoundMode(rmNearest);
    end;
   nValor := RoundTo(Valor, Casas);
  end;
 Result := nValor;
end;

procedure TfrmDemo_ACBrNFSe.btnGerarEnviarNFSeClick(Sender: TObject);
var
 vNumRPS, sNomeArq : String;
begin
 //**************************************************************************
 //
 // A function Gerar só esta disponivel para alguns provedores.
 //
 //**************************************************************************

 if not(InputQuery('Gerar e Enviar NFSe', 'Numero do RPS', vNumRPS))
  then exit;

 ACBrNFSe1.NotasFiscais.Clear;
 GerarNFSe(vNumRPS);

 ACBrNFSe1.Gerar(StrToInt(vNumRPS));
 sNomeArq := ACBrNFSe1.NotasFiscais.Items[0].NomeArq;

 ACBrNFSe1.NotasFiscais.Clear;
 ACBrNFSe1.NotasFiscais.LoadFromFile(sNomeArq);
 ACBrNFSe1.NotasFiscais.Imprimir;

 MemoDados.Lines.Add('Arquivo Carregado de: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
 MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
 LoadXML(MemoResp, WBResposta);
 PageControl2.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrNFSe.btnEnviaremailClick(Sender: TObject);
var
 vAux: String;
 sCC: TStrings;
begin
 OpenDialog1.Title := 'Selecione a NFSe';
 OpenDialog1.DefaultExt := '*-NFSe.xml';
 OpenDialog1.Filter := 'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
 OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Geral.PathSalvar;

 if OpenDialog1.Execute then
  begin
   ACBrNFSe1.NotasFiscais.Clear;
   ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

   if not(InputQuery('Enviar e-mail', 'Destinatário', vAux))
    then exit;

   sCC:=TStringList.Create;
   sCC.Clear;  // Usando para add outros e-mail como Com-Cópia

   ACBrNFSe1.NotasFiscais.Items[0].EnviarEmail(edtSmtpHost.Text,
                                               edtSmtpPort.Text,
                                               edtSmtpUser.Text,
                                               edtSmtpPass.Text,
                                               edtEmailRemetente.Text,
                                               vAux,                 // e-mail do destinatário
                                               edtEmailAssunto.Text, // Assunto
                                               mmEmailMsg.Lines,     // Mensagem
                                               cbEmailSSL.Checked,   // SSL
                                               True,                 // Enviar em PDF
                                               sCC,                  // sCC
                                               nil,                  // Anexos
                                               True,                 // Pede Confirmação de Recebimento
                                               True,                 // Aguarda o Envio
                                               edtEmitRazao.Text);   // Nome do remetente

   sCC.Free;

   MemoDados.Lines.Add('Arquivo Carregado de: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
   MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
   LoadXML(MemoResp, WBResposta);
   PageControl2.ActivePageIndex := 1;
  end;
end;

procedure TfrmDemo_ACBrNFSe.btnLinkNFSeClick(Sender: TObject);
var
 vNumNFSe, sCodVerif, sIM, sLink : String;
begin
 if not(InputQuery('Gerar o Link da NFSe', 'Numero da NFSe', vNumNFSe))
  then exit;
 if not(InputQuery('Gerar o Link da NFSe', 'Codigo de Verificacao', sCodVerif))
  then exit;
 if not(InputQuery('Gerar o Link da NFSe', 'Inscrição Municipal', sIM))
  then exit;

 sLink := ACBrNFSe1.LinkNFSe(StrToIntDef(vNumNFSe, 0), sCodVerif, sIM);

 MemoResp.Lines.Add('Link Gerado: ' + sLink);
 PageControl2.ActivePageIndex := 0;
end;

procedure TfrmDemo_ACBrNFSe.btnGerarLoteRPSClick(Sender: TObject);
var
 vAux, vNumLote : String;
begin
 //**************************************************************************
 //
 // A function GerarLote apenas gera o XML do lote, assina se necessário
 // e valida, salvando o arquivo com o nome: <lote>-lot-rps.xml na pasta Ger
 // Não ocorre o envio para nenhum webservice.
 //
 //**************************************************************************

 if not(InputQuery('Gerar e Enviar Lote', 'Numero do RPS', vAux))
  then exit;

 if not(InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote))
  then exit;

 ACBrNFSe1.NotasFiscais.Clear;
 GerarNFSe(vAux);
 ACBrNFSe1.GerarLote(vNumLote);

 ShowMessage('Arquivo gerado em: '+ACBrNFSe1.NotasFiscais.Items[0].NomeArq);

 ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmDemo_ACBrNFSe.btnGerarEnviarSincronoClick(Sender: TObject);
var
 vAux, vNumLote : String;
begin
 //**************************************************************************
 //
 // A function EnviarSincrono só esta disponivel para alguns provedores.
 //
 //**************************************************************************

 if not(InputQuery('Gerar e Enviar Lote - Sincrono', 'Numero do RPS', vAux))
  then exit;

 if not(InputQuery('Gerar e Enviar Lote - Sincrono', 'Numero do Lote', vNumLote))
  then exit;

 ACBrNFSe1.NotasFiscais.Clear;
 GerarNFSe(vAux);
 ACBrNFSe1.EnviarSincrono(vNumLote);

 ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmDemo_ACBrNFSe.Button1Click(Sender: TObject);
var
 vAux, provedor : String;
begin
 if not(InputQuery('Informe o código IBGE da cidade com 7 digitos', 'Código:', vAux))
  then exit;

 provedor := CodCidadeToProvedor(StrToIntDef(vAux, 0));

 ShowMessage('Provedor: ' + provedor);
end;

end.
