unit Frm_Demo_ACBrBPe;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, ShellAPI, ACBrBase, ACBrDFe, ACBrBPe, Spin, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, SynMemo, SynEdit, ACBrMail, ACBrUtil,
  pcnConversao, ACBrBPeDABPEClass, ACBrBPeDABPeESCPOS, ACBrPosPrinter;

type

  { Tfrm_DemoACBrBPe }

  Tfrm_DemoACBrBPe = class(TForm)
    ACBrBPe1: TACBrBPe;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    PageControl3: TPageControl;
    tsBPe: TTabSheet;
    btnImprimir: TButton;
    btnConsultar: TButton;
    btnValidarXML: TButton;
    btnStatusServ: TButton;
    btnCancBPe: TButton;
    btnCriarEnviar: TButton;
    btnGerarBPE: TButton;
    btnGerarPDF: TButton;
    btnEnviarEmail: TButton;
    btnConsultarChave: TButton;
    btnCancelarChave: TButton;
    btnAdicionarProtBPe: TButton;
    btnCarregarXMLEnviar: TButton;
    btnValidarAssinatura: TButton;
    btnImprimirEvento: TButton;
    btnEnviarEvento: TButton;
    btnDistribuicaoDFe: TButton;
    btnValidarRegrasNegocio: TButton;
    pgRespostas: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TSynMemo;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwBPe: TTreeView;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    btnSalvarConfig: TBitBtn;
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
    sbtnListaCert: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    btnValidadeData: TButton;
    btnNumSerie: TButton;
    btnSubjectName: TButton;
    btnCNPJ: TButton;
    btnIssuerName: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    btnSHA_RSA: TButton;
    cbAssinar: TCheckBox;
    btnHTTPS: TButton;
    btnX509: TButton;
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
    sbPathBPe: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathBPe: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathBPe: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDABPE: TRadioGroup;
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
    ACBrMail1: TACBrMail;
    ACBrBPeDABPeESCPOS1: TACBrBPeDABPeESCPOS;
    GroupBox2: TGroupBox;
    Label30: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    btSerial: TBitBtn;
    cbxModeloPosPrinter: TComboBox;
    cbxPorta: TComboBox;
    cbxPagCodigo: TComboBox;
    seColunas: TSpinEdit;
    seEspLinhas: TSpinEdit;
    seLinhasPular: TSpinEdit;
    ACBrPosPrinter1: TACBrPosPrinter;

    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure btnStatusServClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnGerarBPEClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarChaveClick(Sender: TObject);
    procedure btnValidarRegrasNegocioClick(Sender: TObject);
    procedure btnCancBPeClick(Sender: TObject);
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure btnAdicionarProtBPeClick(Sender: TObject);
    procedure btnCarregarXMLEnviarClick(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnEnviarEventoClick(Sender: TObject);
    procedure btnDistribuicaoDFeClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbPathBPeClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure btnValidadeDataClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubjectNameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnSHA_RSAClick(Sender: TObject);
    procedure btnHTTPSClick(Sender: TObject);
    procedure btnX509Click(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
    procedure sbtnListaCertClick(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure ACBrBPe1StatusChange(Sender: TObject);
    procedure ACBrBPe1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure AtualizaSSLLibsCombo;
    procedure GerarBPe(NumBPe : String);
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TSynMemo);
    procedure PrepararImpressao;
  public
    { Public declarations }
  end;

var
  frm_DemoACBrBPe: Tfrm_DemoACBrBPe;

implementation

uses
  strutils, math, TypInfo, DateUtils, Grids, synacode, blcksock, FileCtrl,
  ufrmStatus, Unit2, ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL,
  pcnConversaoBPe, ACBrBPeBilhetes, ConfiguraSerial;

const
  SELDIRHELP = 1000;

{$R *.lfm}

{ Tfrm_DemoACBrBPe }

procedure Tfrm_DemoACBrBPe.AtualizaSSLLibsCombo;
begin
 cbSSLLib.ItemIndex := Integer( ACBrBPe1.Configuracoes.Geral.SSLLib );
 cbCryptLib.ItemIndex := Integer( ACBrBPe1.Configuracoes.Geral.SSLCryptLib );
 cbHttpLib.ItemIndex := Integer( ACBrBPe1.Configuracoes.Geral.SSLHttpLib );
 cbXmlSignLib.ItemIndex := Integer( ACBrBPe1.Configuracoes.Geral.SSLXmlSignLib );

 cbSSLType.Enabled := (ACBrBPe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure Tfrm_DemoACBrBPe.GerarBPe(NumBPe: String);
Begin
 with ACBrBPe1.Bilhetes.Add.BPe do
  begin
   //
   // Dados de Identificação do BP-e
   //
   Ide.cUF := UFtoCUF(edtEmitUF.Text);

   // TpcnTipoAmbiente = (taProducao, taHomologacao);
   case rgTipoAmb.ItemIndex of
    0: Ide.tpAmb := taProducao;
    1: Ide.tpAmb := taHomologacao;
   end;

   Ide.modelo  := 63;
   Ide.serie   := 1;
   Ide.nBP    := StrToIntDef(NumBPe, 0);
   Ide.cBP    := StrToIntDef(NumBPe, 0);  //Caso não seja preenchido será gerado um número aleatório pelo componente
   // ( moRodoviario, moAquaviario, moFerroviario );
   Ide.modal   := moRodoviario;
   Ide.dhEmi   := Now;
   // TpcnTipoEmissao = (teNormal, teOffLine);
   Ide.tpEmis  := teNormal;
   Ide.verProc := '1.0.0.0'; //Versão do seu sistema
   Ide.indPres := pcPresencial;
   Ide.UFIni   := 'SP';
   Ide.cMunIni := 3503208;
   Ide.UFFim   := 'SP';
   Ide.cMunFim := 3550308;
//   Ide.dhCont  := Now;
//   Ide.xJust   := 'Motivo da Contingência';

   //
   // Dados do Emitente
   //
   Emit.CNPJ  := edtEmitCNPJ.Text;
   Emit.IE    := edtEmitIE.Text;
   Emit.IEST  := '';
   Emit.xNome := edtEmitRazao.Text;
   Emit.xFant := edtEmitFantasia.Text;
   Emit.IM    := '123';
   Emit.CNAE  := '1234567';
   Emit.CRT   := crtRegimeNormal;
   Emit.TAR   := '';

   Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
   Emit.EnderEmit.Nro     := edtEmitNumero.Text;
   Emit.EnderEmit.xCpl    := edtEmitComp.Text;
   Emit.EnderEmit.xBairro := edtEmitBairro.Text;
   Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
   Emit.EnderEmit.xMun    := edtEmitCidade.Text;
   Emit.EnderEmit.CEP     := StrToIntDef(edtEmitCEP.Text, 0);
   Emit.EnderEmit.UF      := edtEmitUF.Text;
   Emit.EnderEmit.fone    := edtEmitFone.Text;
   Emit.enderEmit.email   := 'endereco@provedor.com.br';

   //
   // Dados do Comprador
   //
   Comp.xNome   := 'Nome do Comprador';
   Comp.CNPJCPF := '12345678901';
   Comp.IE      := '';

   Comp.EnderComp.xLgr    := 'Nome do Logradouro';
   Comp.EnderComp.Nro     := 'Numero';
   Comp.EnderComp.xCpl    := 'Complemento';
   Comp.EnderComp.xBairro := 'Bairro';
   Comp.EnderComp.cMun    := 3503208; //StrToInt('Codigo IBGE da cidade do comprador');
   Comp.EnderComp.xMun    := 'Nome da Cidade';
   Comp.EnderComp.CEP     := StrToIntDef('00000000', 0);
   Comp.EnderComp.UF      := 'SP';
   Comp.EnderComp.cPais   := 1058;
   Comp.EnderComp.xPais   := 'BRASIL';
   Comp.EnderComp.fone    := 'Telefone do comprador';
   Comp.enderComp.email   := 'endereco@provedor.com.br';

   //
   // Dados da Agencia
   //
   Agencia.xNome := 'Nome da Agencia';
   Agencia.CNPJ  := edtEmitCNPJ.Text;

   Agencia.EnderAgencia.xLgr    := 'Nome do Logradouro';
   Agencia.EnderAgencia.Nro     := 'Numero';
   Agencia.EnderAgencia.xCpl    := 'Complemento';
   Agencia.EnderAgencia.xBairro := 'Bairro';
   Agencia.EnderAgencia.cMun    := 3503208; //StrToInt('Codigo IBGE da cidade da Agencia');
   Agencia.EnderAgencia.xMun    := 'Nome da Cidade';
   Agencia.EnderAgencia.CEP     := StrToIntDef('00000000', 0);
   Agencia.EnderAgencia.UF      := 'SP';
   Agencia.EnderAgencia.fone    := 'Telefone da Agencia';
   Agencia.enderAgencia.email   := 'endereco@provedor.com.br';
   Agencia.EnderAgencia.cPais   := 1058;
   Agencia.EnderAgencia.xPais   := 'BRASIL';

   //
   // Informações do BP-e Substituido (informar se ocorrer)
   //
//   infBPeSub.chBPe := 'Chave do BPe substituido';
//   infBPeSub.tpSub := tsRemarcacao;

   //
   // Informações sobre a Passagem
   //
   infPassagem.cLocOrig := '1234567'; // Codigo da Localidade de Origem
   infPassagem.xLocOrig := 'Descrição da Localidade de Origem';
   infPassagem.cLocDest := '1234567'; // Codigo da Localidade de Destino
   infPassagem.xLocDest := 'Descrição da Localidade de Destino';
   infPassagem.dhEmb    := Now;
   //
   // Informações sobre o Passageiro
   //
   infPassagem.infPassageiro.xNome := 'Nome do Passageiro';
   infPassagem.infPassageiro.CPF   := '12345679901';
   infPassagem.infPassageiro.tpDoc := tdRG;
   infPassagem.infPassageiro.nDoc  := '12345678'; // Numero do documento
//   infPassagem.infPassageiro.dNasc := StrToDate('10/10/1970');
   infPassagem.infPassageiro.Fone  := '33445566'; // telefone do passageiro
   infPassagem.infPassageiro.Email := 'passageiro@provedor.com.br';

   //
   // Informações sobre a Viagem
   //

   with infViagem.Add do
   begin
     cPercurso    := 'Código do Percurso';
     xPercurso    := 'Descrição do Percurso';
     tpViagem     := tvRegular;
     tpServ       := tsConvencionalComSanitario;
     tpAcomodacao := taAssento;
     tpTrecho     := ttNormal;
//     dhConexao    := ** Informar se o tpTrecho for ttConexao
     prefixo      := 'Prefixo da linha';
     Poltrona     := 21;
     dhViagem     := now; 
     //
     // Informações sobre a Travessia (se ocorrer)
     //
//     infTravessia.tpVeiculo  := tvAutomovel;
//     infTravessia.sitVeiculo := svCarregado;
   end;

   //
   // Informações sobre o valor do BPe
   //

   infValorBPe.vBP        :=  98.00;
   infValorBPe.vDesconto  :=   0.00;
   infValorBPe.vPgto      := 100.00;
   infValorBPe.vTroco     :=   2.00;
   infValorBPe.tpDesconto := tdNenhum;
   infValorBPe.xDesconto  := '';
   //
   // Composição do valor do BPe
   //
   with infValorBPe.Comp.Add do
   begin
     tpComp := tcTarifa;
     vComp  := 25.00;
   end;
   with infValorBPe.Comp.Add do
   begin
     tpComp := tcPedagio;
     vComp  := 35.00;
   end;
   with infValorBPe.Comp.Add do
   begin
     tpComp := tcOutros;
     vComp  := 38.00;
   end;

   //
   // Informações sobre o valor do BPe
   //

   Imp.ICMS.CST   := cst00;
   Imp.ICMS.vBC   := 98.00;
   Imp.ICMS.pICMS := 18.00;
   Imp.ICMS.vICMS := 17.64;

   Imp.vTotTrib   := 0.00;
   Imp.infAdFisco := '';

   //
   // Informações sobre o Pagamento
   //

   with Pag.Add do
   begin
     tPag := fpDinheiro;
     vPag := 98.00;

     tpIntegra := tiNaoInformado;
     CNPJ      := '';
     tBand     := bcOutros;
     cAut      := '';
   end;

   //
   // Autorizados para o Download do XML do BPe
   //
   (*
   with autXML.Add do
   begin
     CNPJCPF := '00000000000000';
   end;

   with autXML.Add do
   begin
     CNPJCPF := '11111111111111';
   end;
   *)
   //
   // Informações Adicionais
   //

   infAdic.infAdFisco := '';
   infAdic.infCpl     := 'Informações Complementares';
  end;

// ACBrBPe1.Bilhetes.GerarBPe;
end;

procedure Tfrm_DemoACBrBPe.GravarConfiguracao;
var
 IniFile: String;
 Ini: TIniFile;
 StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
    Ini.WriteInteger( 'Certificado', 'SSLLib',     cbSSLLib.ItemIndex);
    Ini.WriteInteger( 'Certificado', 'CryptLib',   cbCryptLib.ItemIndex);
    Ini.WriteInteger( 'Certificado', 'HttpLib',    cbHttpLib.ItemIndex);
    Ini.WriteInteger( 'Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString(  'Certificado', 'Caminho',    edtCaminho.Text);
    Ini.WriteString(  'Certificado', 'Senha',      edtSenha.Text);
    Ini.WriteString(  'Certificado', 'NumSerie',   edtNumSerie.Text);

    Ini.WriteBool(    'Geral', 'AtualizarXML',     ckSalvar.Checked);
    Ini.WriteBool(    'Geral', 'ExibirErroSchema', ckSalvar.Checked);
    Ini.WriteString(  'Geral', 'FormatoAlerta',    edtFormatoAlerta.Text);
    Ini.WriteInteger( 'Geral', 'FormaEmissao',     cbFormaEmissao.ItemIndex);
    Ini.WriteInteger( 'Geral', 'VersaoDF',         cbVersaoDF.ItemIndex);
    Ini.WriteBool(    'Geral', 'RetirarAcentos',   cbxRetirarAcentos.Checked);
    Ini.WriteBool(    'Geral', 'Salvar',           ckSalvar.Checked);
    Ini.WriteString(  'Geral', 'PathSalvar',       edtPathLogs.Text);
    Ini.WriteString(  'Geral', 'PathSchemas',      edtPathSchemas.Text);

    Ini.WriteString(  'WebService', 'UF',         cbUF.Text);
    Ini.WriteInteger( 'WebService', 'Ambiente',   rgTipoAmb.ItemIndex);
    Ini.WriteBool(    'WebService', 'Visualizar', cbxVisualizar.Checked);
    Ini.WriteBool(    'WebService', 'SalvarSOAP', cbxSalvarSOAP.Checked);
    Ini.WriteBool(    'WebService', 'AjustarAut', cbxAjustarAut.Checked);
    Ini.WriteString(  'WebService', 'Aguardar',   edtAguardar.Text);
    Ini.WriteString(  'WebService', 'Tentativas', edtTentativas.Text);
    Ini.WriteString(  'WebService', 'Intervalo',  edtIntervalo.Text);
    Ini.WriteInteger( 'WebService', 'TimeOut',    seTimeOut.Value);
    Ini.WriteInteger( 'WebService', 'SSLType',    cbSSLType.ItemIndex);

    Ini.WriteString( 'Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString( 'Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString( 'Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString( 'Proxy', 'Pass',  edtProxySenha.Text);

    Ini.WriteBool(   'Arquivos', 'Salvar',           cbxSalvarArqs.Checked);
    Ini.WriteBool(   'Arquivos', 'PastaMensal',      cbxPastaMensal.Checked);
    Ini.WriteBool(   'Arquivos', 'AddLiteral',       cbxAdicionaLiteral.Checked);
    Ini.WriteBool(   'Arquivos', 'EmissaoPathBPe',   cbxEmissaoPathBPe.Checked);
    Ini.WriteBool(   'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(   'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(   'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString( 'Arquivos', 'PathBPe',          edtPathBPe.Text);
    Ini.WriteString( 'Arquivos', 'PathEvento',       edtPathEvento.Text);

    Ini.WriteString( 'Emitente', 'CNPJ',        edtEmitCNPJ.Text);
    Ini.WriteString( 'Emitente', 'IE',          edtEmitIE.Text);
    Ini.WriteString( 'Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString( 'Emitente', 'Fantasia',    edtEmitFantasia.Text);
    Ini.WriteString( 'Emitente', 'Fone',        edtEmitFone.Text);
    Ini.WriteString( 'Emitente', 'CEP',         edtEmitCEP.Text);
    Ini.WriteString( 'Emitente', 'Logradouro',  edtEmitLogradouro.Text);
    Ini.WriteString( 'Emitente', 'Numero',      edtEmitNumero.Text);
    Ini.WriteString( 'Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString( 'Emitente', 'Bairro',      edtEmitBairro.Text);
    Ini.WriteString( 'Emitente', 'CodCidade',   edtEmitCodCidade.Text);
    Ini.WriteString( 'Emitente', 'Cidade',      edtEmitCidade.Text);
    Ini.WriteString( 'Emitente', 'UF',          edtEmitUF.Text);

    Ini.WriteString( 'Email', 'Host',    edtSmtpHost.Text);
    Ini.WriteString( 'Email', 'Port',    edtSmtpPort.Text);
    Ini.WriteString( 'Email', 'User',    edtSmtpUser.Text);
    Ini.WriteString( 'Email', 'Pass',    edtSmtpPass.Text);
    Ini.WriteString( 'Email', 'Assunto', edtEmailAssunto.Text);
    Ini.WriteBool(   'Email', 'SSL',     cbEmailSSL.Checked );

    StreamMemo := TMemoryStream.Create;
    mmEmailMsg.Lines.SaveToStream(StreamMemo);
    StreamMemo.Seek(0,soFromBeginning);
    Ini.WriteBinaryStream( 'Email', 'Mensagem', StreamMemo);
    StreamMemo.Free;

    Ini.WriteInteger( 'DABPE', 'Tipo',      rgTipoDaBPe.ItemIndex);
    Ini.WriteString(  'DABPE', 'LogoMarca', edtLogoMarca.Text);

    INI.WriteInteger( 'PosPrinter', 'Modelo',            cbxModeloPosPrinter.ItemIndex);
    INI.WriteString(  'PosPrinter', 'Porta',             cbxPorta.Text);
    INI.WriteInteger( 'PosPrinter', 'PaginaDeCodigo',    cbxPagCodigo.ItemIndex);
    INI.WriteString(  'PosPrinter', 'ParamsString',      ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger( 'PosPrinter', 'Colunas',           seColunas.Value);
    INI.WriteInteger( 'PosPrinter', 'EspacoLinhas',      seEspLinhas.Value);
    INI.WriteInteger( 'PosPrinter', 'LinhasEntreCupons', seLinhasPular.Value);
  finally
    Ini.Free;
  end;
end;

procedure Tfrm_DemoACBrBPe.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  Ok: Boolean;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');
  Ini := TIniFile.Create( IniFile );

  try
    cbSSLLib.ItemIndex     := Ini.ReadInteger( 'Certificado', 'SSLLib',     0);
    cbCryptLib.ItemIndex   := Ini.ReadInteger( 'Certificado', 'CryptLib',   0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger( 'Certificado', 'HttpLib',    0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger( 'Certificado', 'XmlSignLib', 0);
    edtCaminho.Text        := Ini.ReadString(  'Certificado', 'Caminho',    '');
    edtSenha.Text          := Ini.ReadString(  'Certificado', 'Senha',      '');
    edtNumSerie.Text       := Ini.ReadString(  'Certificado', 'NumSerie',   '');

    ACBrBPe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
    ACBrBPe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
    ACBrBPe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

    cbxAtualizarXML.Checked     := Ini.ReadBool(    'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(    'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString(  'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger( 'Geral', 'FormaEmissao',     0);
    cbVersaoDF.ItemIndex        := Ini.ReadInteger( 'Geral', 'VersaoDF',         0);
    ckSalvar.Checked            := Ini.ReadBool(    'Geral', 'Salvar',           True);
    cbxRetirarAcentos.Checked   := Ini.ReadBool(    'Geral', 'RetirarAcentos',   True);
    edtPathLogs.Text            := Ini.ReadString(  'Geral', 'PathSalvar',       PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text         := Ini.ReadString(  'Geral', 'PathSchemas',      PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoBPe), integer(cbVersaoDF.ItemIndex) ));

    ACBrBPe1.SSL.DescarregarCertificado;

    with ACBrBPe1.Configuracoes.Geral do
    begin
      SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

      AtualizaSSLLibsCombo;

      ExibirErroSchema := cbxExibirErroSchema.Checked;
      RetirarAcentos   := cbxRetirarAcentos.Checked;
      FormatoAlerta    := edtFormatoAlerta.Text;
      FormaEmissao     := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);
      VersaoDF         := TVersaoBPe(cbVersaoDF.ItemIndex);
      Salvar           := ckSalvar.Checked;
    end;

    cbUF.ItemIndex := cbUF.Items.IndexOf(Ini.ReadString( 'WebService', 'UF', 'SP'));

    rgTipoAmb.ItemIndex   := Ini.ReadInteger( 'WebService', 'Ambiente',   0);
    cbxVisualizar.Checked := Ini.ReadBool(    'WebService', 'Visualizar', False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(    'WebService', 'SalvarSOAP', False);
    cbxAjustarAut.Checked := Ini.ReadBool(    'WebService', 'AjustarAut', False);
    edtAguardar.Text      := Ini.ReadString(  'WebService', 'Aguardar',   '0');
    edtTentativas.Text    := Ini.ReadString(  'WebService', 'Tentativas', '5');
    edtIntervalo.Text     := Ini.ReadString(  'WebService', 'Intervalo',  '0');
    seTimeOut.Value       := Ini.ReadInteger( 'WebService', 'TimeOut',    5000);
    cbSSLType.ItemIndex   := Ini.ReadInteger( 'WebService', 'SSLType',    0);

    edtProxyHost.Text  := Ini.ReadString( 'Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString( 'Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString( 'Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString( 'Proxy', 'Pass',  '');

    with ACBrBPe1.Configuracoes.WebServices do
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
        edtIntervalo.Text := IntToStr(ACBrBPe1.Configuracoes.WebServices.IntervaloTentativas);

      TimeOut   := seTimeOut.Value;
      ProxyHost := edtProxyHost.Text;
      ProxyPort := edtProxyPorta.Text;
      ProxyUser := edtProxyUser.Text;
      ProxyPass := edtProxySenha.Text;
    end;

    ACBrBPe1.SSL.SSLType := TSSLType( cbSSLType.ItemIndex );

    cbxSalvarArqs.Checked       := Ini.ReadBool(   'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked      := Ini.ReadBool(   'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(   'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPathBPe.Checked   := Ini.ReadBool(   'Arquivos', 'EmissaoPathBPe',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(   'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(   'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(   'Arquivos', 'SepararPorModelo', false);
    edtPathBPe.Text             := Ini.ReadString( 'Arquivos', 'PathBPe',          '');
    edtPathEvento.Text          := Ini.ReadString( 'Arquivos', 'PathEvento',       '');

    with ACBrBPe1.Configuracoes.Arquivos do
    begin
      Salvar            := cbxSalvarArqs.Checked;
      SepararPorMes     := cbxPastaMensal.Checked;
      AdicionarLiteral  := cbxAdicionaLiteral.Checked;
      EmissaoPathBPe    := cbxEmissaoPathBPe.Checked;
      SalvarEvento      := cbxSalvaPathEvento.Checked;
      SepararPorCNPJ    := cbxSepararPorCNPJ.Checked;
      SepararPorModelo  := cbxSepararPorModelo.Checked;
      PathSalvar        := edtPathLogs.Text;
      PathSchemas       := edtPathSchemas.Text;
      PathBPe           := edtPathBPe.Text;
      PathEvento        := edtPathEvento.Text;
    end;

    edtEmitCNPJ.Text       := Ini.ReadString( 'Emitente', 'CNPJ',        '');
    edtEmitIE.Text         := Ini.ReadString( 'Emitente', 'IE',          '');
    edtEmitRazao.Text      := Ini.ReadString( 'Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString( 'Emitente', 'Fantasia',    '');
    edtEmitFone.Text       := Ini.ReadString( 'Emitente', 'Fone',        '');
    edtEmitCEP.Text        := Ini.ReadString( 'Emitente', 'CEP',         '');
    edtEmitLogradouro.Text := Ini.ReadString( 'Emitente', 'Logradouro',  '');
    edtEmitNumero.Text     := Ini.ReadString( 'Emitente', 'Numero',      '');
    edtEmitComp.Text       := Ini.ReadString( 'Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString( 'Emitente', 'Bairro',      '');
    edtEmitCodCidade.Text  := Ini.ReadString( 'Emitente', 'CodCidade',   '');
    edtEmitCidade.Text     := Ini.ReadString( 'Emitente', 'Cidade',      '');
    edtEmitUF.Text         := Ini.ReadString( 'Emitente', 'UF',          '');

    edtSmtpHost.Text     := Ini.ReadString( 'Email', 'Host',    '');
    edtSmtpPort.Text     := Ini.ReadString( 'Email', 'Port',    '');
    edtSmtpUser.Text     := Ini.ReadString( 'Email', 'User',    '');
    edtSmtpPass.Text     := Ini.ReadString( 'Email', 'Pass',    '');
    edtEmailAssunto.Text := Ini.ReadString( 'Email', 'Assunto', '');
    cbEmailSSL.Checked   := Ini.ReadBool(   'Email', 'SSL',     False);

    StreamMemo := TMemoryStream.Create;
    Ini.ReadBinaryStream( 'Email', 'Mensagem', StreamMemo);
    mmEmailMsg.Lines.LoadFromStream(StreamMemo);
    StreamMemo.Free;

    rgTipoDaBPe.ItemIndex := Ini.ReadInteger( 'DABPe', 'Tipo',      0);
    edtLogoMarca.Text     := Ini.ReadString(  'DABPe', 'LogoMarca', '');

    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger( 'PosPrinter', 'Modelo',            Integer(ACBrPosPrinter1.Modelo));
    cbxPorta.Text                 := INI.ReadString(  'PosPrinter', 'Porta',             ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex        := INI.ReadInteger( 'PosPrinter', 'PaginaDeCodigo',    Integer(ACBrPosPrinter1.PaginaDeCodigo));
    seColunas.Value               := INI.ReadInteger( 'PosPrinter', 'Colunas',           ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value             := INI.ReadInteger( 'PosPrinter', 'EspacoLinhas',      ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value           := INI.ReadInteger( 'PosPrinter', 'LinhasEntreCupons', ACBrPosPrinter1.LinhasEntreCupons);

    ACBrPosPrinter1.Device.ParamsString := INI.ReadString( 'PosPrinter', 'ParamsString', '');

    if ACBrBPe1.DABPe <> nil then
    begin
      ACBrBPe1.DABPe.TipoDABPe := StrToTpImp(OK,IntToStr(rgTipoDaBPe.ItemIndex+1));
      ACBrBPe1.DABPe.Logo      := edtLogoMarca.Text;
    end;
  finally
     Ini.Free;
  end;
end;

procedure Tfrm_DemoACBrBPe.LoadXML(MyMemo: TMemo; MyWebBrowser: TSynMemo);
var
  vText: String;
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
  //MyWebBrowser.Text := ACBrStrToAnsi(MyMemo.Lines.Text);
end;

procedure Tfrm_DemoACBrBPe.PathClick(Sender: TObject);
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

procedure Tfrm_DemoACBrBPe.sbtnCaminhoCertClick(Sender: TObject);
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

procedure Tfrm_DemoACBrBPe.sbtnLogoMarcaClick(Sender: TObject);
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

procedure Tfrm_DemoACBrBPe.sbtnPathSalvarClick(Sender: TObject);
begin
 PathClick(edtPathLogs);
end;

procedure Tfrm_DemoACBrBPe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoBPe;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
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
  cbFormaEmissao.Items[0] := 'teNormal';
  cbFormaEmissao.ItemIndex := 0;

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoBPe) to High(TVersaoBPe) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoBPe), integer(K) ) );
  cbVersaoDF.Items[0] := 've100';
  cbVersaoDF.ItemIndex := 0;

  cbxModeloPosPrinter.Items.Clear ;
  for N := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModeloPosPrinter.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(N) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For O := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(O) ) ) ;


  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  cbxPorta.Items.Add('LPT1');
  cbxPorta.Items.Add('LPT2');
  cbxPorta.Items.Add('/dev/ttyS0');
  cbxPorta.Items.Add('/dev/ttyS1');
  cbxPorta.Items.Add('/dev/ttyUSB0');
  cbxPorta.Items.Add('/dev/ttyUSB1');
  cbxPorta.Items.Add('\\localhost\Epson');
  cbxPorta.Items.Add('c:\temp\ecf.txt');
  cbxPorta.Items.Add('/tmp/ecf.txt');

  LerConfiguracao;
  PageControl3.ActivePage := tsBPe;
  pgRespostas.ActivePageIndex := 2;

  ACBrBPe1.Configuracoes.WebServices.Salvar := true;
end;

procedure Tfrm_DemoACBrBPe.btnSalvarConfigClick(Sender: TObject);
begin
 GravarConfiguracao;
 LerConfiguracao;
end;

procedure Tfrm_DemoACBrBPe.btnStatusServClick(Sender: TObject);
begin
 ACBrBPe1.WebServices.StatusServico.Executar;

 MemoResp.Lines.Text := ACBrBPe1.WebServices.StatusServico.RetWS;
 memoRespWS.Lines.Text := ACBrBPe1.WebServices.StatusServico.RetornoWS;
 LoadXML(memoRespWS, WBResposta);

 pgRespostas.ActivePageIndex := 1;

 MemoDados.Lines.Add('');
 MemoDados.Lines.Add('Status Serviço');
 MemoDados.Lines.Add('tpAmb: '    +TpAmbToStr(ACBrBPe1.WebServices.StatusServico.tpAmb));
 MemoDados.Lines.Add('verAplic: ' +ACBrBPe1.WebServices.StatusServico.verAplic);
 MemoDados.Lines.Add('cStat: '    +IntToStr(ACBrBPe1.WebServices.StatusServico.cStat));
 MemoDados.Lines.Add('xMotivo: '  +ACBrBPe1.WebServices.StatusServico.xMotivo);
 MemoDados.Lines.Add('cUF: '      +IntToStr(ACBrBPe1.WebServices.StatusServico.cUF));
 MemoDados.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrBPe1.WebServices.StatusServico.dhRecbto));
 MemoDados.Lines.Add('tMed: '     +IntToStr(ACBrBPe1.WebServices.StatusServico.TMed));
 MemoDados.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrBPe1.WebServices.StatusServico.dhRetorno));
 MemoDados.Lines.Add('xObs: '     +ACBrBPe1.WebServices.StatusServico.xObs);
end;

procedure Tfrm_DemoACBrBPe.btnCriarEnviarClick(Sender: TObject);
var
 vAux, vNumLote : String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Bilhete', vAux)) then
    exit;

  if not(InputQuery('WebServices Enviar', 'Numero do Lote', vNumLote)) then
    exit;

  vNumLote := OnlyNumber(vNumLote);

  if Trim(vNumLote) = '' then
   begin
     MessageDlg('Número do Lote inválido.',mtError,[mbok],0);
     exit;
   end;

  ACBrBPe1.Bilhetes.Clear;

  ACBrBPe1.Configuracoes.Geral.VersaoDF :=  TVersaoBPe(cbVersaoDF.ItemIndex);
  GerarBPe(vAux);

  ACBrBPe1.Bilhetes.GerarBPe;

  ACBrBPe1.Enviar(vNumLote, True);

  MemoResp.Lines.Text := ACBrBPe1.WebServices.Enviar.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.Enviar.RetornoWS;
  LoadXML(memoRespWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Envio BPe');
  MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrBPe1.WebServices.Enviar.TpAmb));
  MemoDados.Lines.Add('verAplic: '+ ACBrBPe1.WebServices.Enviar.verAplic);
  MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrBPe1.WebServices.Enviar.cStat));
  MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrBPe1.WebServices.Enviar.cUF));
  MemoDados.Lines.Add('xMotivo: '+ ACBrBPe1.WebServices.Enviar.xMotivo);
  MemoDados.Lines.Add('Protocolo: '+ ACBrBPe1.WebServices.Enviar.BPeRetorno.ProtBPe.Items[0].nProt);

  ACBrBPe1.Bilhetes.Clear;
end;

procedure Tfrm_DemoACBrBPe.btnGerarBPEClick(Sender: TObject);
var
 vAux : String;
begin
if not(InputQuery('WebServices Enviar', 'Numero do Bilhete', vAux)) then
    exit;

  ACBrBPe1.Bilhetes.Clear;

  GerarBPe(vAux);

  ACBrBPe1.Bilhetes.Assinar;

  ACBrBPe1.Bilhetes.Items[0].GravarXML();
  ShowMessage('Arquivo gerado em: '+ACBrBPe1.Bilhetes.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: '+ACBrBPe1.Bilhetes.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrBPe1.Bilhetes.Items[0].NomeArq);
  LoadXML(MemoResp, WBResposta);
  pgRespostas.ActivePageIndex := 1;
end;

procedure Tfrm_DemoACBrBPe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
    ACBrBPe1.Consultar;
    ShowMessage(ACBrBPe1.WebServices.Consulta.Protocolo);
    MemoResp.Lines.Text := ACBrBPe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.Consulta.RetornoWS;
    LoadXML(memoRespWS, WBResposta);
  end;
end;

procedure Tfrm_DemoACBrBPe.btnConsultarChaveClick(Sender: TObject);
var
 vChave : String;
begin
  if not(InputQuery('WebServices Consultar', 'Chave da NF-e:', vChave)) then
    exit;

  ACBrBPe1.Bilhetes.Clear;
  ACBrBPe1.WebServices.Consulta.BPeChave := vChave;
  ACBrBPe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text := ACBrBPe1.WebServices.Consulta.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.Consulta.RetornoWS;
  LoadXML(memoRespWS, WBResposta);
end;

procedure Tfrm_DemoACBrBPe.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg : String;
  Inicio: TDateTime;
  Ok: Boolean;
  Tempo: String;
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
    Inicio := Now;
    Ok := ACBrBPe1.Bilhetes.ValidarRegrasdeNegocios(Msg);
    Tempo := FormatDateTime('hh:nn:ss:zzz', Now - Inicio);

    if not Ok then
    begin
      MemoDados.Lines.Add('Erro: '+Msg);
      ShowMessage('Erros encontrados'+ sLineBreak + 'Tempo: '+Tempo);
    end
    else
      ShowMessage('Tudo OK'+sLineBreak + 'Tempo: '+Tempo);
  end;
end;

procedure Tfrm_DemoACBrBPe.btnCancBPeClick(Sender: TObject);
var
  idLote,vAux : String;
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
       exit;
    ACBrBPe1.EventoBPe.Evento.Clear;
    ACBrBPe1.EventoBPe.idLote := StrToInt(idLote);
    with ACBrBPe1.EventoBPe.Evento.Add do
    begin
     infEvento.dhEvento := now;
     infEvento.tpEvento := teCancelamento;
     infEvento.detEvento.xJust := vAux;
    end;
    ACBrBPe1.EnviarEvento(StrToInt(idLote));

    MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
    LoadXML(memoRespWS, WBResposta);
    ShowMessage(IntToStr(ACBrBPe1.WebServices.EnvEvento.cStat));
    ShowMessage(ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetinfEvento.nProt);
  end;
end;

procedure Tfrm_DemoACBrBPe.btnCancelarChaveClick(Sender: TObject);
var
 Chave, idLote, CNPJ, Protocolo, Justificativa : string;
begin
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Chave da NF-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Cancelamento', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  Protocolo:='';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Protocolo de Autorização', Protocolo)) then
     exit;
  Justificativa := 'Justificativa do Cancelamento';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa do Cancelamento', Justificativa)) then
     exit;

  ACBrBPe1.EventoBPe.Evento.Clear;
  with ACBrBPe1.EventoBPe.Evento.Add do
   begin
     infEvento.chBPe := Chave;
     infEvento.CNPJ   := CNPJ;
     infEvento.dhEvento := now;
     infEvento.tpEvento := teCancelamento;
     infEvento.detEvento.xJust := Justificativa;
     infEvento.detEvento.nProt := Protocolo;
   end;
  ACBrBPe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(memoRespWS, WBResposta);
end;

procedure Tfrm_DemoACBrBPe.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

// Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrBPe1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrBPe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
   begin
     ACBrBPe1.Bilhetes.Clear;
     ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
     try
        ACBrBPe1.Bilhetes.Validar;
        if ACBrBPe1.Bilhetes.Items[0].Alertas <> '' then
          MemoDados.Lines.Add('Alertas: '+ACBrBPe1.Bilhetes.Items[0].Alertas);
        ShowMessage('Bilhete de Passagem Eletrônico Valido');
     except
        on E: Exception do
        begin
          pgRespostas.ActivePage := Dados;
          MemoDados.Lines.Add('Exception: '+E.Message);
          MemoDados.Lines.Add('Erro: '+ACBrBPe1.Bilhetes.Items[0].ErroValidacao);
          MemoDados.Lines.Add('Erro Completo: '+ACBrBPe1.Bilhetes.Items[0].ErroValidacaoCompleto);
        end;
     end;
   end;
end;

procedure Tfrm_DemoACBrBPe.btnGerarPDFClick(Sender: TObject);
var
 CarregarMaisXML : Boolean;
begin
	CarregarMaisXML := true;
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  ACBrBPe1.Bilhetes.Clear;

  while CarregarMaisXML do
  begin
    if OpenDialog1.Execute then
      ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);

    CarregarMaisXML := MessageDlg('Carregar mais Bilhetes?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;

  ACBrBPe1.Bilhetes.ImprimirPDF;
end;

procedure Tfrm_DemoACBrBPe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    PrepararImpressao;

    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
    ACBrBPe1.Bilhetes.Imprimir;
  end;
end;

procedure Tfrm_DemoACBrBPe.btnEnviarEmailClick(Sender: TObject);
var
 Para : String;
 CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
    CC:=TstringList.Create;
    try
      CC.Add('andrefmoraes@gmail.com'); //especifique um email valido
      CC.Add('anfm@zipmail.com.br');    //especifique um email valido

      ACBrMail1.Host := edtSmtpHost.Text;
      ACBrMail1.Port := edtSmtpPort.Text;
      ACBrMail1.Username := edtSmtpUser.Text;
      ACBrMail1.Password := edtSmtpPass.Text;
      ACBrMail1.From := edtSmtpUser.Text;
      ACBrMail1.SetSSL := cbEmailSSL.Checked; // SSL - Conexao Segura
      ACBrMail1.SetTLS := cbEmailSSL.Checked; // Auto TLS
      ACBrMail1.ReadingConfirmation := False; //Pede confirmação de leitura do email
      ACBrMail1.UseThread := False;           //Aguarda Envio do Email(não usa thread)
      ACBrMail1.FromName := 'Projeto ACBr - ACBrBPe';

      ACBrBPe1.Bilhetes.Items[0].EnviarEmail( Para, edtEmailAssunto.Text,
                                               mmEmailMsg.Lines
                                               , True  // Enviar PDF junto
                                               , CC    // Lista com emails que serão enviado cópias - TStrings
                                               , nil); // Lista de anexos - TStrings
    finally
      CC.Free;
    end;
  end;
end;

procedure Tfrm_DemoACBrBPe.btnAdicionarProtBPeClick(Sender: TObject);
var
  NomeArq : String;
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
    ACBrBPe1.Consultar;
    ShowMessage(ACBrBPe1.WebServices.Consulta.Protocolo);
    MemoResp.Lines.Text := ACBrBPe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.Consulta.RetornoWS;
    LoadXML(memoRespWS, WBResposta);
    NomeArq := OpenDialog1.FileName;
    if pos(UpperCase('-bpe.xml'),UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq,'-bpe.xml','-procBPe.xml',[rfIgnoreCase]);
    ACBrBPe1.Bilhetes.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: '+NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: '+NomeArq);
  end;
end;

procedure Tfrm_DemoACBrBPe.btnCarregarXMLEnviarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);

    ACBrBPe1.Enviar(1,True);

    MemoResp.Lines.Text := ACBrBPe1.WebServices.Enviar.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.Enviar.RetornoWS;
    LoadXML(memoRespWS, WBResposta);

   MemoDados.Lines.Add('');
   MemoDados.Lines.Add('Envio BPe');
   MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrBPe1.WebServices.Enviar.TpAmb));
   MemoDados.Lines.Add('verAplic: '+ ACBrBPe1.WebServices.Enviar.verAplic);
   MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrBPe1.WebServices.Enviar.cStat));
   MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrBPe1.WebServices.Enviar.cUF));
   MemoDados.Lines.Add('xMotivo: '+ ACBrBPe1.WebServices.Enviar.xMotivo);
   MemoDados.Lines.Add('Protocolo: '+ ACBrBPe1.WebServices.Enviar.BPeRetorno.ProtBPe.Items[0].nProt);
  end;
end;

procedure Tfrm_DemoACBrBPe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg : String;
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*-bpe.xml';
  OpenDialog1.Filter := 'Arquivos BPe (*-bpe.xml)|*-bpe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
    pgRespostas.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrBPe1.Bilhetes.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrBPe1.SSL.CarregarCertificadoPublico( ACBrBPe1.Bilhetes[0].BPe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrBPe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrBPe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrBPe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure Tfrm_DemoACBrBPe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*.xml';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.xml';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.EventoBPe.Evento.Clear;
    ACBrBPe1.EventoBPe.LerXML(OpenDialog1.FileName) ;
    ACBrBPe1.ImprimirEvento;
  end;
end;

procedure Tfrm_DemoACBrBPe.btnEnviarEventoClick(Sender: TObject);
var
 Para : String;
 CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o BPe';
  OpenDialog1.DefaultExt := '*.xml';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);
  end;

  OpenDialog1.Title := 'Selecione ao Evento';
  OpenDialog1.DefaultExt := '*.xml';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    Evento := TStringList.Create;
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);
    ACBrBPe1.EventoBPe.Evento.Clear;
    ACBrBPe1.EventoBPe.LerXML(OpenDialog1.FileName);
    CC:=TstringList.Create;
    CC.Add('andrefmoraes@gmail.com'); //especifique um email valido
    CC.Add('anfm@zipmail.com.br');    //especifique um email valido

    ACBrBPe1.EnviarEmailEvento(Para,
                               edtEmailAssunto.Text,
                               mmEmailMsg.Lines,
                               CC,
                               Evento,
                               nil);
    CC.Free;
    Evento.Free;
  end;
end;

procedure Tfrm_DemoACBrBPe.btnDistribuicaoDFeClick(Sender: TObject);
var
 cUFAutor, CNPJ, ultNSU, ANSU: string;
begin
  cUFAutor := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'Código da UF do Autor', cUFAutor)) then
     exit;

  CNPJ := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
     exit;

  ultNSU := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'Último NSU recebido pelo ator', ultNSU)) then
     exit;

  ANSU := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'NSU específico', ANSU)) then
     exit;

   ACBrBPe1.DistribuicaoDFe(StrToInt(cUFAutor),CNPJ,ultNSU,ANSU);

  MemoResp.Lines.Text := ACBrBPe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.DistribuicaoDFe.RetornoWS;
  LoadXML(memoRespWS, WBResposta);

  ACBrBPe1.Free;
end;

procedure Tfrm_DemoACBrBPe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrBPe1.SSL.SelecionarCertificado;
end;

procedure Tfrm_DemoACBrBPe.sbPathBPeClick(Sender: TObject);
begin
 PathClick(edtPathBPe);
end;

procedure Tfrm_DemoACBrBPe.sbPathEventoClick(Sender: TObject);
begin
 PathClick(edtPathEvento);
end;

procedure Tfrm_DemoACBrBPe.spPathSchemasClick(Sender: TObject);
begin
 PathClick(edtPathSchemas);
end;

procedure Tfrm_DemoACBrBPe.btnValidadeDataClick(Sender: TObject);
begin
  ShowMessage( FormatDateBr(ACBrBPe1.SSL.CertDataVenc) );
end;

procedure Tfrm_DemoACBrBPe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage( ACBrBPe1.SSL.CertNumeroSerie );
end;

procedure Tfrm_DemoACBrBPe.btnSubjectNameClick(Sender: TObject);
begin
  ShowMessage( ACBrBPe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
               'Razão Social: '+ACBrBPe1.SSL.CertRazaoSocial);
end;

procedure Tfrm_DemoACBrBPe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage( ACBrBPe1.SSL.CertCNPJ );
end;

procedure Tfrm_DemoACBrBPe.btnSHA_RSAClick(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrBPe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure Tfrm_DemoACBrBPe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrBPe1.SSL.UseCertificateHTTP;
  ACBrBPe1.SSL.UseCertificateHTTP := False;
  try
    MemoResp.Lines.Text := ACBrBPe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrBPe1.SSL.UseCertificateHTTP := OldUseCert;
  end;
  pgRespostas.ActivePageIndex := 0;
end;

procedure Tfrm_DemoACBrBPe.btnX509Click(Sender: TObject);
begin
  with ACBrBPe1.SSL do
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

procedure Tfrm_DemoACBrBPe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage( ACBrBPe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: '+ACBrBPe1.SSL.CertCertificadora);
end;

procedure Tfrm_DemoACBrBPe.sbtnListaCertClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    ACBrBPe1.SSL.LerCertificadosStore;
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

    for I := 0 to ACBrBPe1.SSL.ListaCertificados.Count-1 do
    begin
      with ACBrBPe1.SSL.ListaCertificados[I] do
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

procedure Tfrm_DemoACBrBPe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure Tfrm_DemoACBrBPe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure Tfrm_DemoACBrBPe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure Tfrm_DemoACBrBPe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure Tfrm_DemoACBrBPe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrBPe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure Tfrm_DemoACBrBPe.ACBrBPe1StatusChange(Sender: TObject);
begin
  case ACBrBPe1.Status of
    stIdleBPe :
    begin
      if ( frmStatus <> nil ) then
        frmStatus.Hide;
    end;
    stBPeStatusServico :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stBPeRecepcao :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando dados da BPe...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stBPeRetRecepcao :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Recebendo dados da BPe...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stBPeConsulta :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Consultando BPe...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stBPeEmail :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando Email...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stBPeEvento :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando Evento...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
  end;
  Application.ProcessMessages;
end;

procedure Tfrm_DemoACBrBPe.ACBrBPe1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
 memoLog.Lines.Add(ALogLine);
end;

procedure Tfrm_DemoACBrBPe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure Tfrm_DemoACBrBPe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/35');
end;

procedure Tfrm_DemoACBrBPe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure Tfrm_DemoACBrBPe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure Tfrm_DemoACBrBPe.lblMouseEnter(Sender: TObject);
begin
 TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure Tfrm_DemoACBrBPe.lblMouseLeave(Sender: TObject);
begin
 TLabel(Sender).Font.Style := [fsBold];
end;

procedure Tfrm_DemoACBrBPe.btSerialClick(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
       cbxPorta.Text := frConfiguraSerial.Device.Porta ;
       ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
     FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure Tfrm_DemoACBrBPe.PrepararImpressao;
begin
  ACBrPosPrinter1.Desativar;

  ACBrPosPrinter1.Modelo         := TACBrPosPrinterModelo( cbxModeloPosPrinter.ItemIndex );
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
  ACBrPosPrinter1.Porta          := cbxPorta.Text;

  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
  ACBrPosPrinter1.LinhasEntreCupons  := seLinhasPular.Value;
  ACBrPosPrinter1.EspacoEntreLinhas  := seEspLinhas.Value;

//  ACBrBPeDABPeESCPOS1.ImprimeQRCode     := True;
//  ACBrBPeDABPeESCPOS1.ImprimeEmUmaLinha := cbImprimir1Linha.Checked;
end;

end.
