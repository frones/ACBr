unit Frm_ACBrNFSe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase, ACBrUtil, ACBrDFe, ACBrDFeReport, ACBrMail, ACBrNFSeDANFSeClass,
  ACBrNFSeDANFSeRLClass, ACBrNFSe;

type
  TfrmACBrNFSe = class(TForm)
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
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbFormaEmissao: TComboBox;
    cbxAtualizarXML: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
    cbxRetirarAcentos: TCheckBox;
    edtPathSchemas: TEdit;
    TabSheet7: TTabSheet;
    GroupBox4: TGroupBox;
    lTimeOut: TLabel;
    lSSLLib1: TLabel;
    cbxVisualizar: TCheckBox;
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
    TabSheet13: TTabSheet;
    sbPathNFSe: TSpeedButton;
    Label35: TLabel;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathNFSe: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathNFSe: TEdit;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDANFSE: TRadioGroup;
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
    tsCancelamento: TTabSheet;
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
    OpenDialog1: TOpenDialog;
    ACBrNFSe1: TACBrNFSe;
    ACBrNFSeDANFSeRL1: TACBrNFSeDANFSeRL;
    ACBrMail1: TACBrMail;
    Label30: TLabel;
    edtSenhaWeb: TEdit;
    Label33: TLabel;
    edtUserWeb: TEdit;
    Label34: TLabel;
    edtFraseSecWeb: TEdit;
    Label6: TLabel;
    lblSchemas: TLabel;
    Label41: TLabel;
    edtArqINI: TEdit;
    sbtArqINI: TSpeedButton;
    Label39: TLabel;
    edtPrestLogo: TEdit;
    sbtnPrestLogo: TSpeedButton;
    Label40: TLabel;
    edtPrefeitura: TEdit;
    Label21: TLabel;
    cbCidades: TComboBox;
    Label22: TLabel;
    edtEmitUF: TEdit;
    Label20: TLabel;
    edtCodCidade: TEdit;
    Label32: TLabel;
    edtTotalCidades: TEdit;
    edtEmitCidade: TEdit;
    Label42: TLabel;
    edtEmailRemetente: TEdit;
    cbEmailTLS: TCheckBox;
    btnGerarEnviarLote: TButton;
    btnGerarEnviarNFSe: TButton;
    btnGerarEnviarSincrono: TButton;
    btnImprimir: TButton;
    btnEnviaremail: TButton;
    btnLinkNFSe: TButton;
    btnGerarLoteRPS: TButton;
    btnSubsNFSe: TButton;
    btnConsultarSitLote: TButton;
    btnConsultarLote: TButton;
    btnConsultarNFSeRPS: TButton;
    btnConsultarNFSePeriodo: TButton;
    btnCancNFSe: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathNFSeClick(Sender: TObject);
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
    procedure sbtnLogoMarcaClick(Sender: TObject);
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
    procedure ACBrNFSe1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure ACBrNFSe1StatusChange(Sender: TObject);
    procedure sbtnPrestLogoClick(Sender: TObject);
    procedure sbtArqINIClick(Sender: TObject);
    procedure btnGerarEnviarLoteClick(Sender: TObject);
    procedure btnGerarEnviarNFSeClick(Sender: TObject);
    procedure btnGerarEnviarSincronoClick(Sender: TObject);
    procedure btnGerarLoteRPSClick(Sender: TObject);
    procedure btnSubsNFSeClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnEnviaremailClick(Sender: TObject);
    procedure btnLinkNFSeClick(Sender: TObject);
    procedure btnConsultarSitLoteClick(Sender: TObject);
    procedure btnConsultarLoteClick(Sender: TObject);
    procedure btnConsultarNFSeRPSClick(Sender: TObject);
    procedure btnConsultarNFSePeriodoClick(Sender: TObject);
    procedure btnCancNFSeClick(Sender: TObject);
    procedure cbCidadesChange(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure AlimentarNFSe(NumDFe, NumLote: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
    procedure AtualizarCidades;
    function RoundTo5(Valor: Double; Casas: Integer): Double;
  public
    { Public declarations }
  end;

var
  frmACBrNFSe: TfrmACBrNFSe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  pcnAuxiliar, pnfsNFSe, pcnConversao, pnfsConversao,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBrNFSeNotasFiscais, ACBrNFSeConfiguracoes,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrNFSe }

procedure TfrmACBrNFSe.ACBrNFSe1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrNFSe.ACBrNFSe1StatusChange(Sender: TObject);
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
    stNFSeConsultaSituacao:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Consultando a Situação...';
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

procedure TfrmACBrNFSe.AlimentarNFSe(NumDFe, NumLote: String);
var
  ValorISS: Double;
begin
  with ACBrNFSe1 do
  begin
    // Provedor ISSNet sem certificado
    // Configuracoes.Geral.Emitente.WebChaveAcesso := 'A001.B0001.C0001-1';

    // Provedor Sigep sem certificado
//    Configuracoes.Geral.Emitente.WebChaveAcesso := 'A001.B0001.C0001';

    // Provedor iiBrasil token = WebChaveAcesso para homologação
//    Configuracoes.Geral.Emitente.WebChaveAcesso := 'TLXX4JN38KXTRNSEAJYYEA==';
    Configuracoes.Geral.Emitente.WebChaveAcesso := 'TLXX4JN38KXTRNSE';

    with Configuracoes.Geral.Emitente.DadosSenhaParams.Add do
    begin
      Param := 'ChaveAutorizacao';
      Conteudo := 'A001.B0001.C0001-1';
    end;

    NotasFiscais.NumeroLote := NumLote;
    NotasFiscais.Transacao := True;

    with NotasFiscais.Add.NFSe do
    begin
      ChaveNFSe := '123456789012345678901234567890123456789';
      Numero := NumDFe;
      SeriePrestacao := '1';

      NumeroLote := NumLote;

      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumDFe));

      // Para o provedor ISS.NET em ambiente de Homologação mudar a série para '8'
      IdentificacaoRps.Serie := 'NF'; //'85';

      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      IdentificacaoRps.Tipo := trRPS;

      DataEmissao := Date;
      DataEmissaoRPS := Date;
      (*
        TnfseNaturezaOperacao = ( no1, no2, no3, no4, no5, no6, no7,
        no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
        no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
        no70, no71, no72, no78, no79,
        no101, no111, no121, no201, no301,
        no501, no511, no541, no551, no601, no701 );
      *)
      NaturezaOperacao := no1;
      // NaturezaOperacao := no51;

      // TnfseRegimeEspecialTributacao = ( retNenhum, retMicroempresaMunicipal, retEstimativa, retSociedadeProfissionais, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP );
       RegimeEspecialTributacao := retNenhum;
//      RegimeEspecialTributacao := retLucroReal;

      // TnfseSimNao = ( snSim, snNao );
      OptanteSimplesNacional := snNao;

      // TnfseSimNao = ( snSim, snNao );
      IncentivadorCultural := snNao;
      // Provedor Tecnos
      PercentualCargaTributaria := 0;
      ValorCargaTributaria := 0;
      PercentualCargaTributariaMunicipal := 0;
      ValorCargaTributariaMunicipal := 0;
      PercentualCargaTributariaEstadual := 0;
      ValorCargaTributariaEstadual := 0;

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

      // Usado quando o RPS for substituir outro
      // RpsSubstituido.Numero := FormatFloat('#########0', i);
      // RpsSubstituido.Serie  := 'UNICA';
      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      /// RpsSubstituido.Tipo   := trRPS;

      Servico.Valores.ValorServicos := 100.00;
      Servico.Valores.ValorDeducoes := 0.00;
      Servico.Valores.ValorPis := 0.00;
      Servico.Valores.ValorCofins := 0.00;
      Servico.Valores.ValorInss := 0.00;
      Servico.Valores.ValorIr := 0.00;
      Servico.Valores.ValorCsll := 0.00;

      // TnfseSituacaoTributaria = ( stRetencao, stNormal, stSubstituicao );
      // stRetencao = snSim
      // stNormal   = snNao

      // Neste exemplo não temos ISS Retido ( stNormal = Não )
      // Logo o valor do ISS Retido é igual a zero.
      Servico.Valores.IssRetido := stNormal;
      Servico.Valores.ValorIssRetido := 0.00;

      Servico.Valores.OutrasRetencoes := 0.00;
      Servico.Valores.DescontoIncondicionado := 0.00;
      Servico.Valores.DescontoCondicionado := 0.00;

      Servico.Valores.BaseCalculo := Servico.Valores.ValorServicos -
        Servico.Valores.ValorDeducoes - Servico.Valores.DescontoIncondicionado;
      // No caso do provedor Ginfes devemos informar a aliquota já dividida por 100
      // para outros provedores devemos informar por exemplo 3, mas ao fazer o calculo
      // do valor do ISS devemos dividir por 100
      Servico.Valores.Aliquota := 4;

      // Valor do ISS calculado multiplicando-se a base de calculo pela aliquota
      ValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota / 100;

      // A função RoundTo5 é usada para arredondar valores, sendo que o segundo
      // parametro se refere ao numero de casas decimais.
      // exemplos: RoundTo5(50.532, -2) ==> 50.53
      // exemplos: RoundTo5(50.535, -2) ==> 50.54
      // exemplos: RoundTo5(50.536, -2) ==> 50.54

      Servico.Valores.ValorISS := RoundTo5(ValorISS, -2);

      Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
        Servico.Valores.ValorPis - Servico.Valores.ValorCofins -
        Servico.Valores.ValorInss - Servico.Valores.ValorIr -
        Servico.Valores.ValorCsll - Servico.Valores.OutrasRetencoes -
        Servico.Valores.ValorIssRetido - Servico.Valores.DescontoIncondicionado
        - Servico.Valores.DescontoCondicionado;

      // TnfseResponsavelRetencao = ( ptTomador, rtPrestador );
      Servico.ResponsavelRetencao := ptTomador;

      Servico.ItemListaServico := '09.01';
//      Servico.CodigoCnae := '452000200';
      Servico.CodigoCnae := '852010';

      // Usado pelo provedor de Goiania
      Servico.CodigoTributacaoMunicipio := '09.01';

      // Para o provedor ISS.NET em ambiente de Homologação
      // o Codigo CNAE tem que ser '6511102'
      // Servico.CodigoCnae                := '123'; // Informação Opcional
      // Servico.CodigoTributacaoMunicipio := '3314799';
      Servico.Discriminacao := 'discriminacao I;discriminacao II';

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
        codLCServ := '123';
        Descricao := 'SERVICO 1';
        Quantidade := 1;
        ValorUnitario := 15.00;
        ValorServicos := Quantidade * ValorUnitario;
      end;

      Prestador.CNPJ := edtEmitCNPJ.Text; //'88888888888888';
      Prestador.InscricaoMunicipal := edtEmitIM.Text;

      // Para o provedor ISSDigital deve-se informar também:
      Prestador.Senha := edtSenhaWeb.Text;
      Prestador.FraseSecreta := edtFraseSecWeb.Text;
      Prestador.cUF := 33;

      // Provedor WebFisco
      Prestador.Usuario := StrToIntDef(ACBrNFSe1.Configuracoes.Geral.Emitente.WebUser, 0);
      Prestador.CNPJ_Prefeitura := '12.345.678/0001-23';

      PrestadorServico.Endereco.Endereco := edtEmitLogradouro.Text;
      PrestadorServico.Endereco.Numero   := edtEmitNumero.Text;
      PrestadorServico.Endereco.Bairro   := edtEmitBairro.Text;
      PrestadorServico.Endereco.CodigoMunicipio := edtCodCidade.Text;
      PrestadorServico.Endereco.UF := edtEmitUF.Text;
      PrestadorServico.Endereco.CodigoPais := 1058;
      PrestadorServico.Endereco.xPais := 'BRASIL';

      PrestadorServico.Endereco.CEP := '14800123';

      PrestadorServico.RazaoSocial  := edtEmitRazao.Text;
      PrestadorServico.NomeFantasia := edtEmitRazao.Text;

      PrestadorServico.Contato.Telefone := '1633224455';

      Tomador.IdentificacaoTomador.CpfCnpj := '55555555555555';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '17331600';

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE';

      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.Bairro := 'CENTRO';
      Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Tomador.Endereco.UF := edtEmitUF.Text;
      Tomador.Endereco.CodigoPais := 1058; // Brasil
      Tomador.Endereco.CEP := edtEmitCEP.Text;
      // Provedor Equiplano é obrigatório o pais e IE
      Tomador.Endereco.xPais := 'BRASIL';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '123456';

      Tomador.Contato.Telefone := '22223333';
      Tomador.Contato.Email := 'nome@provedor.com.br';

      Tomador.AtualizaTomador := snNao;
      Tomador.TomadorExterior := snNao;

      // Usado quando houver um intermediario na prestação do serviço
      // IntermediarioServico.RazaoSocial        := 'razao';
      // IntermediarioServico.CpfCnpj            := '00000000000';
      // IntermediarioServico.InscricaoMunicipal := '12547478';

      // Usado quando o serviço for uma obra
      // ConstrucaoCivil.CodigoObra := '88888';
      // ConstrucaoCivil.Art        := '433';
    end;
  end;
end;

procedure TfrmACBrNFSe.AtualizarCidades;
var
  IniFile: String;
  Ini: TIniFile;
  Cidades: TStringList;
  I: Integer;
  sNome, sCod, sUF: String;
begin
  IniFile := edtArqINI.Text+'\Cidades.ini';
  Ini := TIniFile.Create(IniFile);
  Cidades := TStringList.Create;

  try
    Ini.ReadSections(Cidades);
    cbCidades.Items.Clear;
    for I := 0 to Pred(Cidades.Count) do
      if (StrToIntdef(Cidades[I], 0) > 0) then
        begin
          //Exemplo: Alfenas/3101607/MG
          sCod := Cidades[I];
          sNome := Ini.ReadString(sCod, 'Nome', '');
          sUF := Ini.ReadString(sCod, 'UF', '');

          cbCidades.Items.Add(Format('%s/%s/%s', [sNome, sCod, sUF]));
        end;

    //Sort
    cbCidades.Sorted := false;
    cbCidades.Sorted := true;
    edtTotalCidades.Text := IntToStr(cbCidades.Items.Count);
  finally
    FreeAndNil(Cidades);
  end;
end;

procedure TfrmACBrNFSe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrNFSe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrNFSe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrNFSe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrNFSe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrNFSe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrNFSe.btnCancNFSeClick(Sender: TObject);
var
  Codigo, Motivo: String;
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
    // 1 - Erro de emissão
    // 2 - Serviço não concluido
    // 3 - RPS Cancelado na Emissão

    if not (InputQuery('Cancelar NFSe', 'Código de Cancelamento', Codigo)) then
      exit;

    // Provedor Equiplano é obrigatório o motivo de cancelamento
    if not (InputQuery('Cancelar NFSe', 'Motivo de Cancelamento', Motivo)) then
      exit;

//    ACBrNFSe1.NotasFiscais.Items[0].NFSe.MotivoCancelamento:= Motivo;

    ACBrNFSe1.CancelarNFSe(Codigo, '', Motivo);

    MemoDados.Lines.Add('Arquivo Carregado de: ' + ACBrNFSe1.NotasFiscais.Items
      [0].NomeArq);
    MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoDados.Lines.Add('Retorno do Cancelamento:');

    MemoDados.Lines.Add('Cód. Cancelamento: ' +
      ACBrNFSe1.WebServices.CancNfse.CodigoCancelamento);
    if ACBrNFSe1.WebServices.CancNfse.DataHora <> 0 then
      MemoDados.Lines.Add('Data / Hora      : ' +
        DateTimeToStr(ACBrNFSe1.WebServices.CancNfse.DataHora));
    LoadXML(MemoResp.Text, WBResposta);

    pgRespostas.ActivePageIndex := 1;
  end;
end;

procedure TfrmACBrNFSe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertCNPJ);
end;

procedure TfrmACBrNFSe.btnConsultarLoteClick(Sender: TObject);
var
  Lote, Protocolo: String;
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

  if not(InputQuery('Consultar Lote', 'Número do Lote', Lote)) then
    exit;
  if not(InputQuery('Consultar Lote', 'Número do Protocolo', Protocolo)) then
    exit;

  ACBrNFSe1.ConsultarLoteRps(Lote, Protocolo);

  MemoResp.Lines.Text := ACBrNFSe1.WebServices.ConsLote.RetWS;
  memoRespWS.Lines.Text := ACBrNFSe1.WebServices.ConsLote.RetWS;
  LoadXML(ACBrNFSe1.WebServices.ConsLote.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrNFSe.btnConsultarNFSePeriodoClick(Sender: TObject);
var
  DataInicial, DataFinal, NumeroNFSe: String;
begin
  if not(InputQuery('Consultar NFSe por Período', 'Data Inicial (DD/MM/AAAA):',
    DataInicial)) then
    exit;
  if not(InputQuery('Consultar NFSe por Período', 'Data Final (DD/MM/AAAA):',
    DataFinal)) then
    exit;

  if not(InputQuery('Consultar NFSe por Período', 'Numero da NFSe:',
    NumeroNFSe)) then
    exit;

  ACBrNFSe1.ConsultarNFSe(StrToDate(DataInicial), StrToDate(DataFinal), NumeroNFSe);

  MemoResp.Lines.Text := ACBrNFSe1.WebServices.ConsNfse.RetWS;
  memoRespWS.Lines.Text := ACBrNFSe1.WebServices.ConsNfse.RetWS;
  LoadXML(ACBrNFSe1.WebServices.ConsNfse.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrNFSe.btnConsultarNFSeRPSClick(Sender: TObject);
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

    ACBrNFSe1.ConsultarNFSeporRps(ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero,
      ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie,
      TipoRPSToStr(ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo));

    MemoResp.Lines.Text := ACBrNFSe1.WebServices.ConsNfseRps.RetWS;
    memoRespWS.Lines.Text := ACBrNFSe1.WebServices.ConsNfseRps.RetWS;
    LoadXML(ACBrNFSe1.WebServices.ConsNfseRps.RetWS, WBResposta);

    pgRespostas.ActivePageIndex := 1;
  end;
end;

procedure TfrmACBrNFSe.btnConsultarSitLoteClick(Sender: TObject);
var
  Protocolo: String;
begin
  if not(InputQuery('Consultar Situação do Lote', 'Número do Protocolo',
    Protocolo)) then
    exit;

  ACBrNFSe1.ConsultarSituacao(Protocolo);

  MemoResp.Lines.Text := ACBrNFSe1.WebServices.ConsSitLoteRPS.RetWS;
  memoRespWS.Lines.Text := ACBrNFSe1.WebServices.ConsSitLoteRPS.RetWS;
  LoadXML(ACBrNFSe1.WebServices.ConsSitLoteRPS.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrNFSe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrNFSe1.SSL.CertDataVenc));
end;

procedure TfrmACBrNFSe.btnEnviaremailClick(Sender: TObject);
var
  vAux: String;
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

    if not(InputQuery('Enviar e-mail', 'Destinatário', vAux)) then
      exit;

    sCC := TStringList.Create;
    sCC.Clear; // Usando para add outros e-mail como Com-Cópia

    ACBrNFSe1.NotasFiscais.Items[0].EnviarEmail(vAux, edtEmailAssunto.Text,
      mmEmailMsg.Lines, True // Enviar PDF junto
      , nil // Lista com emails que serão enviado cópias - TStrings
      , nil // Lista de anexos - TStrings
      );

    sCC.Free;

    MemoDados.Lines.Add('Arquivo Carregado de: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    LoadXML(MemoResp.Text, WBResposta);

    pgRespostas.ActivePageIndex := 1;
  end;
end;

procedure TfrmACBrNFSe.btnGerarEnviarLoteClick(Sender: TObject);
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
    AlimentarNFSe(sAux, vNumLote);
    inc(iAux);
  end;

  ACBrNFSe1.Enviar(vNumLote);

  for I := 0 to iQtde - 1 do
  begin
    MemoDados.Lines.Add('Nome XML: ' + ACBrNFSe1.NotasFiscais.Items[I].NomeArq);
    MemoDados.Lines.Add('Nota Numero: ' + ACBrNFSe1.NotasFiscais.Items[I]
      .NFSe.Numero);
    MemoDados.Lines.Add('Código de Verificação: ' + ACBrNFSe1.NotasFiscais.Items
      [I].NFSe.CodigoVerificacao);
  end;

  ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmACBrNFSe.btnGerarEnviarNFSeClick(Sender: TObject);
var
  vNumRPS, sNomeArq: String;
begin
  // **************************************************************************
  //
  // A function Gerar só esta disponivel para alguns provedores.
  //
  // **************************************************************************

  if not(InputQuery('Gerar e Enviar NFSe', 'Numero do RPS', vNumRPS)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentarNFSe(vNumRPS, '1');

  ACBrNFSe1.Gerar(StrToInt(vNumRPS));
  sNomeArq := ACBrNFSe1.NotasFiscais.Items[0].NomeArq;

  ACBrNFSe1.NotasFiscais.Clear;
  ACBrNFSe1.NotasFiscais.LoadFromFile(sNomeArq);
  ACBrNFSe1.NotasFiscais.Imprimir;

  MemoDados.Lines.Add('Arquivo Carregado de: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrNFSe.btnGerarEnviarSincronoClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  // **************************************************************************
  //
  // A function EnviarSincrono só esta disponivel para alguns provedores.
  //
  // **************************************************************************

  if not(InputQuery('Gerar e Enviar Lote - Sincrono', 'Numero do RPS', vAux)) then
    exit;

  if not(InputQuery('Gerar e Enviar Lote - Sincrono', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentarNFSe(vAux, vNumLote);
  ACBrNFSe1.EnviarSincrono(vNumLote);

  ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmACBrNFSe.btnGerarLoteRPSClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  // **************************************************************************
  //
  // A function GerarLote apenas gera o XML do lote, assina se necessário
  // e valida, salvando o arquivo com o nome: <lote>-lot-rps.xml na pasta Ger
  // Não ocorre o envio para nenhum webservice.
  //
  // **************************************************************************

  if not(InputQuery('Gerar e Enviar Lote', 'Numero do RPS', vAux)) then
    exit;

  if not(InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentarNFSe(vAux, vNumLote);
  ACBrNFSe1.GerarLote(vNumLote);

  ShowMessage('Arquivo gerado em: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);

  ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmACBrNFSe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrNFSe1.SSL.UseCertificateHTTP;
  ACBrNFSe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrNFSe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrNFSe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.btnImprimirClick(Sender: TObject);
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
    ACBrNFSe1.NotasFiscais.Imprimir;
    ACBrNFSe1.NotasFiscais.ImprimirPDF;

    MemoDados.Lines.Add('Arquivo Carregado de: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoDados.Lines.Add('Nota Numero: ' + ACBrNFSe1.NotasFiscais.Items[0].NFSe.Numero);
    MemoDados.Lines.Add('Código de Verificação: ' + ACBrNFSe1.NotasFiscais.Items[0].NFSe.CodigoVerificacao);
    MemoDados.Lines.Add('Data de Emissão: ' + DateToStr(ACBrNFSe1.NotasFiscais.Items[0].NFSe.DataEmissao));

    MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    LoadXML(MemoResp.Text, WBResposta);

    pgRespostas.ActivePageIndex := 1;
  end;
end;

procedure TfrmACBrNFSe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrNFSe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrNFSe1.SSL.CertCertificadora);
end;

procedure TfrmACBrNFSe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrNFSe1.SSL do
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

procedure TfrmACBrNFSe.btnLinkNFSeClick(Sender: TObject);
var
  vNumNFSe, sCodVerif, sLink: String;
begin
  if not(InputQuery('Gerar o Link da NFSe', 'Numero da NFSe', vNumNFSe)) then
    exit;

  if not(InputQuery('Gerar o Link da NFSe', 'Codigo de Verificacao', sCodVerif)) then
    exit;

  sLink := ACBrNFSe1.LinkNFSe(StrToIntDef(vNumNFSe, 0), sCodVerif);

  MemoResp.Lines.Add('Link Gerado: ' + sLink);
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrNFSe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrNFSe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrNFSe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrNFSe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrNFSe.btnSubsNFSeClick(Sender: TObject);
var
  Codigo, vAux, sNumNFSe: String;
begin
  if not(InputQuery('Substituir NFS-e', 'Numero do novo RPS', vAux)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentarNFSe(vAux, '1');

  // Codigo de Cancelamento
  // 1 - Erro de emissão
  // 2 - Serviço não concluido
  // 3 - RPS Cancelado na Emissão

  if not(InputQuery('Substituir NFSe', 'Código de Cancelamento', Codigo)) then
    exit;

  if not(InputQuery('Substituir NFS-e', 'Numero da NFS-e', sNumNFSe)) then
    exit;

  ACBrNFSe1.SubstituirNFSe(Codigo, sNumNFSe);

  MemoDados.Lines.Add('Retorno da Substituição:');
  MemoDados.Lines.Add('Cód. Cancelamento: ' + ACBrNFSe1.WebServices.SubNfse.CodigoCancelamento);

//  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrNFSe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrNFSe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrNFSe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
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

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrNFSe.GravarConfiguracao;
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
    Ini.WriteBool(   'Geral', 'RetirarAcentos',   cbxRetirarAcentos.Checked);
    Ini.WriteBool(   'Geral', 'Salvar',           ckSalvar.Checked);
    Ini.WriteString( 'Geral', 'PathSalvar',       edtPathLogs.Text);
    Ini.WriteString( 'Geral', 'PathSchemas',      edtPathSchemas.Text);
    Ini.WriteString( 'Geral', 'LogoMarca',        edtLogoMarca.Text);
    Ini.WriteString( 'Geral', 'PrestLogo',        edtPrestLogo.Text);
    Ini.WriteString( 'Geral', 'Prefeitura',       edtPrefeitura.Text);
    Ini.WriteString( 'Geral', 'PathINI',          edtArqINI.Text);

    Ini.WriteInteger('WebService', 'Ambiente',    rgTipoAmb.ItemIndex);
    Ini.WriteBool(   'WebService', 'Visualizar',  cbxVisualizar.Checked);
    Ini.WriteBool(   'WebService', 'SalvarSOAP',  cbxSalvarSOAP.Checked);
    Ini.WriteBool(   'WebService', 'AjustarAut',  cbxAjustarAut.Checked);
    Ini.WriteString( 'WebService', 'Aguardar',    edtAguardar.Text);
    Ini.WriteString( 'WebService', 'Tentativas',  edtTentativas.Text);
    Ini.WriteString( 'WebService', 'Intervalo',   edtIntervalo.Text);
    Ini.WriteInteger('WebService', 'TimeOut',     seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType',     cbSSLType.ItemIndex);
    Ini.WriteString( 'WebService', 'SenhaWeb',    edtSenhaWeb.Text);
    Ini.WriteString( 'WebService', 'UserWeb',     edtUserWeb.Text);
    Ini.WriteString( 'WebService', 'FraseSecWeb', edtFraseSecWeb.Text);

    Ini.WriteString('Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass',  edtProxySenha.Text);

    Ini.WriteBool(  'Arquivos', 'Salvar',          cbxSalvarArqs.Checked);
    Ini.WriteBool(  'Arquivos', 'PastaMensal',     cbxPastaMensal.Checked);
    Ini.WriteBool(  'Arquivos', 'AddLiteral',      cbxAdicionaLiteral.Checked);
    Ini.WriteBool(  'Arquivos', 'EmissaoPathNFSe', cbxEmissaoPathNFSe.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',  cbxSepararPorCNPJ.Checked);
    Ini.WriteString('Arquivos', 'PathNFSe',        edtPathNFSe.Text);

    Ini.WriteString('Emitente', 'CNPJ',        edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IM',          edtEmitIM.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia',    edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone',        edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP',         edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro',  edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero',      edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro',      edtEmitBairro.Text);
    Ini.WriteString('Emitente', 'CodCidade',   edtCodCidade.Text);
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

    Ini.WriteInteger('DANFSE', 'Tipo',      rgTipoDANFSE.ItemIndex);
    Ini.WriteString( 'DANFSE', 'LogoMarca', edtLogoMarca.Text);

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFSe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrNFSe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrNFSe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrNFSe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrNFSe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrNFSe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrNFSe.LerConfiguracao;
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

    edtArqINI.Text := Ini.ReadString('Geral', 'PathINI', '');
    AtualizarCidades;

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);

    ckSalvar.Checked          := Ini.ReadBool(  'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(  'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString('Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString('Geral', 'PathSchemas',    '');
    edtLogoMarca.Text         := Ini.ReadString('Geral', 'LogoMarca',      '');
    edtPrestLogo.Text         := Ini.ReadString('Geral', 'PrestLogo',      '');
    edtPrefeitura.Text        := Ini.ReadString('Geral', 'Prefeitura',     '');
    edtArqINI.Text            := Ini.ReadString('Geral', 'PathINI',        '');

    rgTipoAmb.ItemIndex   := Ini.ReadInteger('WebService', 'Ambiente',    0);
    cbxVisualizar.Checked := Ini.ReadBool(   'WebService', 'Visualizar',  False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(   'WebService', 'SalvarSOAP',  False);
    cbxAjustarAut.Checked := Ini.ReadBool(   'WebService', 'AjustarAut',  False);
    edtAguardar.Text      := Ini.ReadString( 'WebService', 'Aguardar',    '0');
    edtTentativas.Text    := Ini.ReadString( 'WebService', 'Tentativas',  '5');
    edtIntervalo.Text     := Ini.ReadString( 'WebService', 'Intervalo',   '0');
    seTimeOut.Value       := Ini.ReadInteger('WebService', 'TimeOut',     5000);
    cbSSLType.ItemIndex   := Ini.ReadInteger('WebService', 'SSLType',     0);
    edtSenhaWeb.Text      := Ini.ReadString( 'WebService', 'SenhaWeb',    '');
    edtUserWeb.Text       := Ini.ReadString( 'WebService', 'UserWeb',     '');
    edtFraseSecWeb.Text   := Ini.ReadString( 'WebService', 'FraseSecWeb', '');

    edtProxyHost.Text  := Ini.ReadString('Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString('Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass',  '');

    cbxSalvarArqs.Checked       := Ini.ReadBool(  'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked      := Ini.ReadBool(  'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(  'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPathNFSe.Checked  := Ini.ReadBool(  'Arquivos', 'EmissaoPathNFSe',   false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    edtPathNFSe.Text            := Ini.ReadString('Arquivos', 'PathNFSe',          '');

    edtEmitCNPJ.Text       := Ini.ReadString('Emitente', 'CNPJ',        '');
    edtEmitIM.Text         := Ini.ReadString('Emitente', 'IM',          '');
    edtEmitRazao.Text      := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString('Emitente', 'Fantasia',    '');
    edtEmitFone.Text       := Ini.ReadString('Emitente', 'Fone',        '');
    edtEmitCEP.Text        := Ini.ReadString('Emitente', 'CEP',         '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro',  '');
    edtEmitNumero.Text     := Ini.ReadString('Emitente', 'Numero',      '');
    edtEmitComp.Text       := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString('Emitente', 'Bairro',      '');
    edtCodCidade.Text      := Ini.ReadString('Emitente', 'CodCidade',   '');
    edtEmitCidade.Text     := Ini.ReadString('Emitente', 'Cidade',      '');
    edtEmitUF.Text         := Ini.ReadString('Emitente', 'UF',          '');
    cbCidades.ItemIndex    := cbCidades.Items.IndexOf(edtEmitCidade.Text + '/' +
      edtCodCidade.Text + '/' + edtEmitUF.Text);

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

    rgTipoDANFSe.ItemIndex := Ini.ReadInteger('DANFSE', 'Tipo',       0);
    edtLogoMarca.Text      := Ini.ReadString( 'DANFSE', 'LogoMarca',  '');

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFSe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: String;
begin
  ACBrNFSe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrNFSe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrNFSe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrNFSe1.SSL.DescarregarCertificado;

  with ACBrNFSe1.Configuracoes.Geral do
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

    PathIniCidades  := edtArqINI.Text;
    PathIniProvedor := edtArqINI.Text;
    CodigoMunicipio := StrToIntDef(edtCodCidade.Text, 0);
    SenhaWeb        := edtSenhaWeb.Text;
    UserWeb         := edtUserWeb.Text;

    Emitente.CNPJ         := edtEmitCNPJ.Text;
    Emitente.InscMun      := edtEmitIM.Text;
    Emitente.RazSocial    := edtEmitRazao.Text;
    Emitente.WebUser      := edtUserWeb.Text;
    Emitente.WebSenha     := edtSenhaWeb.Text;
    Emitente.WebFraseSecr := edtFraseSecWeb.Text;

    SetConfigMunicipio;
  end;

  with ACBrNFSe1.Configuracoes.WebServices do
  begin
    Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := cbxSalvarSOAP.Checked;
    UF         := edtEmitUF.Text;

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
      edtIntervalo.Text := IntToStr(ACBrNFSe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrNFSe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrNFSe1.Configuracoes.Arquivos do
  begin
    NomeLongoNFSe    := True;
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathNFSe  := cbxEmissaoPathNFSe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    PathSalvar       := edtPathLogs.Text;
    PathSchemas      := edtPathSchemas.Text;
    PathNFSe         := edtPathNFSe.Text;
    PathGer          := edtPathLogs.Text;
    PathCan          := PathMensal;
    PathMensal       := GetPathGer(0);
    PathSalvar       := PathMensal;
  end;

  if ACBrNFSe1.DANFSe <> nil then
  begin
    // TTipoDANFSE = ( tpPadrao, tpIssDSF, tpFiorilli );
    ACBrNFSe1.DANFSe.TipoDANFSE := tpPadrao;
    ACBrNFSe1.DANFSe.Logo       := edtLogoMarca.Text;
    ACBrNFSe1.DANFSe.PrestLogo  := edtPrestLogo.Text;
    ACBrNFSe1.DANFSe.Prefeitura := edtPrefeitura.Text;
    ACBrNFSe1.DANFSe.PathPDF    := PathMensal;

    ACBrNFSe1.DANFSe.MargemDireita  := 7;
    ACBrNFSe1.DANFSe.MargemEsquerda := 7;
    ACBrNFSe1.DANFSe.MargemSuperior := 5;
    ACBrNFSe1.DANFSe.MargemInferior := 5;
  end;

  with ACBrNFSe1.MAIL do
  begin
    Host      := edtSmtpHost.Text;
    Port      := edtSmtpPort.Text;
    Username  := edtSmtpUser.Text;
    Password  := edtSmtpPass.Text;
    From      := edtEmailRemetente.Text;
    FromName  := edtEmitRazao.Text;
    SetTLS    := cbEmailTLS.Checked;
    SetSSL    := cbEmailSSL.Checked;
    UseThread := False;

    ReadingConfirmation := False;
  end;

  lblSchemas.Caption := ACBrNFSe1.Configuracoes.Geral.xProvedor;
end;

procedure TfrmACBrNFSe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  ACBrUtil.WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ACBrUtil.ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrNFSe1.NotasFiscais.Count > 0then
    MemoResp.Lines.Add('Empresa: ' + ACBrNFSe1.NotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial);
end;

procedure TfrmACBrNFSe.PathClick(Sender: TObject);
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

function TfrmACBrNFSe.RoundTo5(Valor: Double; Casas: Integer): Double;
var
  xValor, xDecimais: String;
  p, nCasas: Integer;
  nValor: Double;
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
    xDecimais := Copy(xValor, p + 1, Length(xValor));
    if Length(xDecimais) > nCasas then
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

procedure TfrmACBrNFSe.sbPathNFSeClick(Sender: TObject);
begin
  PathClick(edtPathNFSe);
end;

procedure TfrmACBrNFSe.sbtArqINIClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(edtArqINI.Text) <= 0 then
    Dir := ExtractFileDir(Application.ExeName)
  else
    Dir := edtArqINI.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], SELDIRHELP)
  then
    edtArqINI.Text := Dir;
end;

procedure TfrmACBrNFSe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFSe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrNFSe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrNFSe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFSe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  ACBrNFSe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrNFSe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrNFSe1.SSL.ListaCertificados[I] do
    begin
      ASerie := NumeroSerie;

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

procedure TfrmACBrNFSe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrNFSe.sbtnPrestLogoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter :=
    'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(Application.ExeName);

  if OpenDialog1.Execute then
  begin
    edtLogoMarca.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmACBrNFSe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TfrmACBrNFSe.cbCidadesChange(Sender: TObject);
var
  Tamanho: Integer;
begin
  Tamanho := Length(Trim(cbCidades.Text));

  edtEmitCidade.Text := Copy(cbCidades.Text, 1, Tamanho - 11);
  edtEmitUF.Text := Copy(cbCidades.Text, Tamanho - 1, 2);
  edtCodCidade.Text := Copy(cbCidades.Text, Tamanho - 9, 7);
end;

end.
