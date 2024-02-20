{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
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

unit Frm_ACBrNFCom;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrDFe, ACBrDFeReport, ACBrBase,
  ACBrNFComDANFComClass, ACBrMail, ACBrNFCom;

type
  TfrmACBrNFCom = class(TForm)
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
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
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
    tsEventos: TTabSheet;
    btnCriarEnviar: TButton;
    btnConsultar: TButton;
    btnConsultarChave: TButton;
    btnValidarRegrasNegocio: TButton;
    btnGerarXML: TButton;
    btnValidarXML: TButton;
    btnEnviarEmail: TButton;
    btnAdicionarProtocolo: TButton;
    btnCarregarXMLEnviar: TButton;
    btnValidarAssinatura: TButton;
    btnCancelarXML: TButton;
    btnCancelarChave: TButton;
    btnImprimirEvento: TButton;
    btnEnviarEventoEmail: TButton;
    pgRespostas: TPageControl;
    TabSheet6: TTabSheet;
    wbXmlRetorno: TWebBrowser;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwDocumento: TTreeView;
    TabSheet10: TTabSheet;
    ACBrMail1: TACBrMail;
    OpenDialog1: TOpenDialog;
    cbxEmissaoPathNFCom: TCheckBox;
    edtPathNFCom: TEdit;
    btnStatusServ: TButton;
    rgTipoDANFCom: TRadioGroup;
    ACBrNFCom1: TACBrNFCom;
    wbXmlEnvio: TWebBrowser;

    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathNFComClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure btnDataValidadeClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubNameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
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
    procedure btnStatusServClick(Sender: TObject);
    procedure btnGerarXMLClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnCarregarXMLEnviarClick(Sender: TObject);
    procedure btnValidarRegrasNegocioClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure btnAdicionarProtocoloClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarChaveClick(Sender: TObject);
    procedure btnCancelarXMLClick(Sender: TObject);
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
    procedure btnImprimirDANFComClick(Sender: TObject);
    procedure btnImprimirDANFComOfflineClick(Sender: TObject);
    procedure ACBrNFCom1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrNFCom1StatusChange(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    Procedure AlimentarComponente(NumDFe: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrNFCom: TfrmACBrNFCom;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.XMLHTML, ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrDFeUtil, ACBrDFeSSL, ACBrDFeOpenSSL,
  ACBrXmlBase,
  pcnConversao,
  ACBrNFComConversao,
  Frm_Status, Frm_SelecionarCertificado, Frm_ConfiguraSerial;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrNFCom }

procedure TfrmACBrNFCom.AlimentarComponente(NumDFe: String);
begin
  ACBrNFCom1.NotasFiscais.Clear;

  with ACBrNFCom1.NotasFiscais.Add.NFCom do
  begin
    // Dados de Identificação do NFCom
    //
    Ide.cUF := UFtoCUF(edtEmitUF.Text);

    // TACBrTipoAmbiente = (taProducao, taHomologacao);
    case rgTipoAmb.ItemIndex of
      0: Ide.tpAmb := TACBrTipoAmbiente.taProducao;
      1: Ide.tpAmb := TACBrTipoAmbiente.taHomologacao;
    end;

    Ide.modelo := 62;
    Ide.serie  := 1;
    Ide.nNF    := StrToIntDef(NumDFe, 0);
    {
      A função GerarCodigoDFe possui 2 parâmetros:
      sendo que o primeiro (obrigatório) é o numero do documento
      e o segundo (opcional) é a quantidade de digitos que o código tem.
      Os valores aceitos para o segundo parâmentros são: 7 ou 8 (padrão)
    }
    Ide.cNF := GerarCodigoDFe(Ide.nNF, 7);

    Ide.dhEmi  := Now;
    // TACBrTipoEmissao = (teNormal, teOffLine);
    Ide.tpEmis  := TACBrTipoEmissao.teNormal;
    Ide.nSiteAutoriz := sa0;
    Ide.cMunFG  := 3503208;
    Ide.finNFCom := fnNormal;
    Ide.verProc := 'ACBrNFCom'; //Versão do seu sistema
    Ide.indPrePago := tiNao;
    Ide.indCessaoMeiosRede := tiNao;
    Ide.indNotaEntrada := tiNao;

    // Alimentar os 2 campos abaixo só em caso de contingência
//   Ide.dhCont  := Now;
//   Ide.xJust   := 'Motivo da Contingência';

    // Dados do
    //
    Emit.CNPJ  := edtEmitCNPJ.Text;
    Emit.IE    := edtEmitIE.Text;
    Emit.xNome := edtEmitRazao.Text;
    Emit.xFant := edtEmitFantasia.Text;

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

    // Dados do Destinatário
    //
    Dest.xNome     := 'Nome do Destinatario';
    Dest.CNPJCPF   := '06760213874';
    Dest.indIEDest := inNaoContribuinte;
    Dest.IE        := '';
    Dest.IM        := '';

    Dest.EnderDest.xLgr    := 'Endereco';
    Dest.EnderDest.Nro     := '123';
    Dest.EnderDest.xCpl    := '';
    Dest.EnderDest.xBairro := 'Centro';
    Dest.EnderDest.cMun    := StrToInt(edtEmitCodCidade.Text);
    Dest.EnderDest.xMun    := 'Cidade';
    Dest.EnderDest.CEP     := 14800000;
    Dest.EnderDest.UF      := 'SP';
    Dest.EnderDest.fone    := '33445566';
    Dest.EnderDest.email   := 'endereco@provedor.com.br';

    // Dados do Assinante
    //
    assinante.iCodAssinante := '123';

    // (taComercial, taIndustrial, taResidencial, taProdutorRural,
    //  taOrgaoPublico, taPrestadorServicoTeleCom, taMissoesDiplomaticas,
    //  taIgrejasTemplos, taOutros);
    assinante.tpAssinante := taResidencial;

    // (suTelefonia, suComunicacaoDados, suTVAssinatura, suAcessoInternet,
    //  suMultimidia, suOutros, suCombo);
    assinante.tpServUtil := suAcessoInternet;

    assinante.nContrato := '123456';
    assinante.dContratoIni := StrToDate('01/01/2024');
    assinante.dContratoFim := StrToDate('01/12/2024');

    // Em se tratando de plano de prestação de serviço telefônico corporativo,
    // familiar ou similares, informar o número do terminal telefônico principal
    // do plano.
    assinante.NroTermPrinc := '';
    assinante.cUFPrinc := 0;

    // Número dos Terminais adicionais do serviço contratado
    {
    with assinante.TermAdic.New do
    begin
      NroTermAdic := '';
      cUFAdic := 0;
    end;
    }
    // Grupo de Informações da Substituição
    gSub.chNFCom := '';
    // ou
    gSub.gNF.CNPJ := '';
    gSub.gNF.Modelo := 21; // 21 ou 22
    gSub.gNF.serie := '';
    gSub.gNF.nNF := 0;
    gSub.gNF.CompetEmis := StrToDate('01/01/2024');
    gSub.gNF.hash115 := '';

    // (msErroPreco, msErroCadastral, msDecisaoJudicial,
    //  msErroTributacao, msDescontServico, msComplValores);
    gSub.motSub := msErroPreco;

    // Grupo de Informações do Cofaturamento
    gCofat.chNFComLocal := '';

    // Detalhamento de Produtos e Servicos
    with Det.New do
    begin
      nItem := 1;
      chNFComAnt := '';
      nItemAnt := 0;

      with Prod do
      begin
        cProd        := '123';
        xProd        := 'Descricao do Produto';
        cClass       := '4562032';
        CFOP         := 1234;
        CNPJLD       := '';

        // (umMinuto, umMB, umGB, umUN);
        uMed         := umMB;
        qFaturada    := 50;
        vItem        := 2;
        vDesc        := 0;
        vOutro       := 0;
        vProd        := qFaturada * vItem;
        dExpiracao   := 0;
        indDevolucao := tiNao;
      end;

      with Imposto do
      begin
        with ICMS do
        begin
          CST   := cst00;
          vBC   := 100;
          pICMS := 18;
          vICMS := vBC * pICMS / 100;
        end;

        with PIS do
        begin
          CST  := pis01;
          vBC  := 100;
          pPIS := 2;
          vPIS := vBC * pPIS / 100;
        end;

        with COFINS do
        begin
          CST     := cof01;
          vBC     := 100;
          pCOFINS := 1.5;
          vCOFINS := vBC * pCOFINS / 100;
        end;

        with FUST do
        begin
          vBC := 0;
          pFUST := 0;
          vFUST := 0;
        end;

        with FUNTTEL do
        begin
          vBC := 0;
          pFUNTTEL := 0;
          vFUNTTEL := 0;
        end;

        with retTrib do
        begin
          vRetPIS := 0;
          vRetCOFINS := 0;
          vRetCSLL := 0;
          vBCIRRF := 0;
          vIRRF := 0;
        end;
      end;

      with gProcRef do
      begin
        vItem := 1;
        qFaturada := 0;
        vProd := 0;
        vDesc := 0;
        vOutro := 0;
        indDevolucao := tiNao;
        vBC := 0;
        pICMS := 0;
        vICMS := 0;
        vPIS := 0;
        vCOFINS := 0;

        with gProc.New do
        begin
          // (tpSEFAZ, tpJusticaFederal, tpJusticaEstadual)
          tpProc := tpSEFAZ;
          nProcesso := '123';
        end;
      end;

      with gRessarc do
      begin
        // (tpCobrancaIndevida, tpInterrupcao, tpOutros)
        tpRessarc := tpCobrancaIndevida;
        dRef := 0;
        nProcesso := '';
        nProtReclama := '';
        xObs := '';
      end;

      infAdProd := '';
    end;

    with Total do
    begin
      vProd      := 100;
      vBC        := 100;
      vICMS      := 18;
      vICMSDeson := 0;
      vFCP       := 0;
      vCOFINS    := 1.5;
      vPIS       := 2;
      vFUNTTEL   := 0;
      vFUST      := 0;
      vRetPIS    := 0;
      vRetCOFINS := 0;
      vRetCSLL   := 0;
      vIRRF      := 0;
      vDesc      := 0;
      vOutro     := 0;
      vNF        := 100;
    end;

    with gFidelidade do
    begin
      qtdSaldoPts := '';
      dRefSaldoPts := 0;
      qtdPtsResg := '';
      dRefResgPts := 0;
    end;

    with gFat do
    begin
      CompetFat := StrToDate('01/01/2024');
      dVencFat := StrToDate('01/02/2024');
      dPerUsoIni := StrToDate('01/12/2023');
      dPerUsoFim := StrToDate('31/12/2023');
      codBarras := '123456789012345678901234567890123456789012345678';
      codDebAuto := '12345678';
      codBanco := '';
      codAgencia := '';

      with enderCorresp do
      begin
        xLgr    := 'Endereco';
        Nro     := '123';
        xCpl    := '';
        xBairro := 'Centro';
        cMun    := StrToInt(edtEmitCodCidade.Text);
        xMun    := 'Cidade';
        CEP     := 14800000;
        UF      := 'SP';
        fone    := '33445566';
        email   := 'endereco@provedor.com.br';
      end;

      gPIX.urlQRCodePIX := '';
    end;

    with gFatCentral do
    begin
      CNPJ := '';
      cUF := 35;
    end;

    // Autorizados para o Download do XML do NFCom
    {
    with autXML.New do
    begin
      CNPJCPF := '23456789000110';
    end;
    }
    // Informações Adicionais
    infAdic.infAdFisco := '';
    infAdic.infCpl := 'Informações Complementares';
    {
    with infRespTec do
    begin
      CNPJ     := '23456789000110';
      xContato := 'Nome do Contato';
      email    := 'nome@provedor.com.br';
      fone     := '33445566';
    end;
    }
  end;

  ACBrNFCom1.NotasFiscais.GerarNFCom;
end;

procedure TfrmACBrNFCom.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrNFCom1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrNFCom1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrNFCom1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrNFCom1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrNFCom1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrNFCom.btnAdicionarProtocoloClick(Sender: TObject);
var
  NomeArq: String;
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFCom1.Consultar;

    ShowMessage(ACBrNFCom1.WebServices.Consulta.Protocolo);

//    MemoResp.Lines.Text := ACBrNFCom1.WebServices.Consulta.RetWS;
//    memoRespWS.Lines.Text := ACBrNFCom1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrNFCom1.WebServices.Consulta.RetornoWS, wbXmlRetorno);
    NomeArq := OpenDialog1.FileName;

    if pos(UpperCase('-NFCom.xml'), UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq, '-NFCom.xml', '-procNFCom.xml', [rfIgnoreCase]);

    ACBrNFCom1.NotasFiscais.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: ' + NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: ' + NomeArq);
  end;
end;

procedure TfrmACBrNFCom.btnCancelarChaveClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
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

  ACBrNFCom1.EventoNFCom.Evento.Clear;

  with ACBrNFCom1.EventoNFCom.Evento.New do
  begin
    infEvento.chNFCom := Chave;
    infEvento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCancelamento;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrNFCom1.EnviarEvento(StrToInt(idLote));

  LoadXML(ACBrNFCom1.WebServices.EnvEvento.RetWS, wbXmlRetorno);
  (*
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chNFCom
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrNFCom.btnCancelarXMLClick(Sender: TObject);
var
  idLote, vAux: String;
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;

    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
       exit;

    ACBrNFCom1.EventoNFCom.Evento.Clear;
    ACBrNFCom1.EventoNFCom.idLote := StrToInt(idLote);

    with ACBrNFCom1.EventoNFCom.Evento.New do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;

    ACBrNFCom1.EnviarEvento(StrToInt(idLote));

//    MemoResp.Lines.Text := ACBrNFCom1.WebServices.EnvEvento.RetWS;
//    memoRespWS.Lines.Text := ACBrNFCom1.WebServices.EnvEvento.RetornoWS;
    LoadXML(ACBrNFCom1.WebServices.EnvEvento.RetornoWS, wbXmlRetorno);
    ShowMessage(IntToStr(ACBrNFCom1.WebServices.EnvEvento.cStat));
    ShowMessage(ACBrNFCom1.WebServices.EnvEvento.EventoRetorno.RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrNFCom.btnCarregarXMLEnviarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    with ACBrNFCom1.NotasFiscais.Items[0].NFCom do
    begin
      Emit.CNPJ              := edtEmitCNPJ.Text;
      Emit.IE                := edtEmitIE.Text;
      Emit.xNome             := edtEmitRazao.Text;
      Emit.xFant             := edtEmitFantasia.Text;

      Emit.EnderEmit.fone    := edtEmitFone.Text;
      Emit.EnderEmit.CEP     := StrToInt(edtEmitCEP.Text);
      Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
      Emit.EnderEmit.nro     := edtEmitNumero.Text;
      Emit.EnderEmit.xCpl    := edtEmitComp.Text;
      Emit.EnderEmit.xBairro := edtEmitBairro.Text;
      Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
      Emit.EnderEmit.xMun    := edtEmitCidade.Text;
      Emit.EnderEmit.UF      := edtEmitUF.Text;
    end;

    ACBrNFCom1.Enviar;

//    MemoResp.Lines.Text := ACBrNFCom1.WebServices.Retorno.RetWS;
//    memoRespWS.Lines.Text := ACBrNFCom1.WebServices.Retorno.RetornoWS;
    LoadXML(ACBrNFCom1.WebServices.Enviar.RetWS, wbXmlRetorno);

    memoLog.Lines.Add('');
    memoLog.Lines.Add('Envio NFCom');
    memoLog.Lines.Add('tpAmb: '+ TipoAmbienteToStr(ACBrNFCom1.WebServices.Enviar.TpAmb));
    memoLog.Lines.Add('verAplic: '+ ACBrNFCom1.WebServices.Enviar.verAplic);
    memoLog.Lines.Add('cStat: '+ IntToStr(ACBrNFCom1.WebServices.Enviar.cStat));
    memoLog.Lines.Add('cUF: '+ IntToStr(ACBrNFCom1.WebServices.Enviar.cUF));
    memoLog.Lines.Add('xMotivo: '+ ACBrNFCom1.WebServices.Enviar.xMotivo);
//    memoLog.Lines.Add('cMsg: '+ IntToStr(ACBrNFCom1.WebServices.Enviar.cMsg));
//    memoLog.Lines.Add('xMsg: '+ ACBrNFCom1.WebServices.Enviar.xMsg);
    memoLog.Lines.Add('Recibo: '+ ACBrNFCom1.WebServices.Enviar.Recibo);
//    memoLog.Lines.Add('Protocolo: '+ ACBrNFCom1.WebServices.Enviar.Protocolo);
  end;
end;

procedure TfrmACBrNFCom.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrNFCom1.SSL.CertCNPJ);
end;

procedure TfrmACBrNFCom.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  vChave := '';
  if not(InputQuery('WebServices Consultar', 'Chave da NFCom:', vChave)) then
    exit;

  ACBrNFCom1.NotasFiscais.Clear;
  ACBrNFCom1.Consultar(vChave);

  LoadXML(ACBrNFCom1.WebServices.Consulta.RetWS, wbXmlRetorno);

  pgRespostas.ActivePageIndex := 0;

  memoLog.Lines.Add('');
  memoLog.Lines.Add('Consultar Situação');
  memoLog.Lines.Add('tpAmb: '    + TipoAmbienteToStr(ACBrNFCom1.WebServices.Consulta.tpAmb));
  memoLog.Lines.Add('verAplic: ' + ACBrNFCom1.WebServices.Consulta.verAplic);
  memoLog.Lines.Add('cStat: '    + IntToStr(ACBrNFCom1.WebServices.Consulta.cStat));
  memoLog.Lines.Add('xMotivo: '  + ACBrNFCom1.WebServices.Consulta.xMotivo);
  memoLog.Lines.Add('cUF: '      + IntToStr(ACBrNFCom1.WebServices.Consulta.cUF));
  memoLog.Lines.Add('dhRecbto: ' + DateTimeToStr(ACBrNFCom1.WebServices.Consulta.dhRecbto));
//  memoLog.Lines.Add('tMed: '     +IntToStr(ACBrNFCom1.WebServices.Consulta.TMed));
//  memoLog.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrNFCom1.WebServices.Consulta.dhRetorno));
//  memoLog.Lines.Add('xObs: '     +ACBrNFCom1.WebServices.Consulta.xObs);
end;

procedure TfrmACBrNFCom.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFCom1.Consultar;

    LoadXML(ACBrNFCom1.WebServices.Consulta.RetWS, wbXmlRetorno);

    pgRespostas.ActivePageIndex := 0;

    memoLog.Lines.Add('');
    memoLog.Lines.Add('Consultar Situação');
    memoLog.Lines.Add('Protocolo: ' + ACBrNFCom1.WebServices.Consulta.Protocolo);
    memoLog.Lines.Add('');
    memoLog.Lines.Add('tpAmb: '     + TipoAmbienteToStr(ACBrNFCom1.WebServices.Consulta.tpAmb));
    memoLog.Lines.Add('verAplic: '  + ACBrNFCom1.WebServices.Consulta.verAplic);
    memoLog.Lines.Add('cStat: '     + IntToStr(ACBrNFCom1.WebServices.Consulta.cStat));
    memoLog.Lines.Add('xMotivo: '   + ACBrNFCom1.WebServices.Consulta.xMotivo);
    memoLog.Lines.Add('cUF: '       + IntToStr(ACBrNFCom1.WebServices.Consulta.cUF));
    memoLog.Lines.Add('dhRecbto: '  + DateTimeToStr(ACBrNFCom1.WebServices.Consulta.dhRecbto));
  //  memoLog.Lines.Add('tMed: '     +IntToStr(ACBrNFCom1.WebServices.Consulta.TMed));
  //  memoLog.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrNFCom1.WebServices.Consulta.dhRetorno));
  //  memoLog.Lines.Add('xObs: '     +ACBrNFCom1.WebServices.Consulta.xObs);
  end;
end;

procedure TfrmACBrNFCom.btnCriarEnviarClick(Sender: TObject);
var
  vAux: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero da Nota', vAux)) then
    exit;

  AlimentarComponente(vAux);

  ACBrNFCom1.Enviar;

  pgRespostas.ActivePageIndex := 0;

  LoadXML(ACBrNFCom1.WebServices.Enviar.RetWS, wbXmlRetorno);

  memoLog.Lines.Add('');
  memoLog.Lines.Add('Envio NFCom');
  memoLog.Lines.Add('tpAmb: ' + TipoAmbienteToStr(ACBrNFCom1.WebServices.Enviar.TpAmb));
  memoLog.Lines.Add('verAplic: ' + ACBrNFCom1.WebServices.Enviar.verAplic);
  memoLog.Lines.Add('cStat: ' + IntToStr(ACBrNFCom1.WebServices.Enviar.cStat));
  memoLog.Lines.Add('cUF: ' + IntToStr(ACBrNFCom1.WebServices.Enviar.cUF));
  memoLog.Lines.Add('xMotivo: ' + ACBrNFCom1.WebServices.Enviar.xMotivo);
  memoLog.Lines.Add('Recibo: '+ ACBrNFCom1.WebServices.Enviar.Recibo);
  (*
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].tpAmb
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].verAplic
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].chNFCom
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].dhRecbto
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].nProt
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].digVal
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].cStat
  ACBrNFCom1.WebServices.Retorno.NFComRetorno.ProtNFCom.Items[0].xMotivo
  *)
end;

procedure TfrmACBrNFCom.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrNFCom1.SSL.CertDataVenc));
end;

procedure TfrmACBrNFCom.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrNFCom1.NotasFiscais.Clear;
  ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrNFCom1.NotasFiscais.Items[0].EnviarEmail( Para, edtEmailAssunto.Text,
                                             mmEmailMsg.Lines
                                             , True  // Enviar PDF junto
                                             , CC    // Lista com emails que serao enviado copias - TStrings
                                             , nil); // Lista de anexos - TStrings
  finally
    CC.Free;
  end;
end;

procedure TfrmACBrNFCom.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione ao Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  Evento := TStringList.Create;
  CC := TStringList.Create;
  try
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);
    ACBrNFCom1.EventoNFCom.Evento.Clear;
    ACBrNFCom1.EventoNFCom.LerXML(OpenDialog1.FileName);

    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrNFCom1.EnviarEmailEvento(Para
      , edtEmailAssunto.Text
      , mmEmailMsg.Lines
      , CC // Lista com emails que serao enviado copias - TStrings
      , nil // Lista de anexos - TStrings
      , nil  // ReplyTo
      );
  finally
    CC.Free;
    Evento.Free;
  end;
end;

procedure TfrmACBrNFCom.btnGerarXMLClick(Sender: TObject);
var
  vAux, Xml: string;
begin
  if not(InputQuery('WebServices Enviar', 'Numero da Nota', vAux)) then
    exit;

  ACBrNFCom1.NotasFiscais.Clear;

  AlimentarComponente(vAux);

  ACBrNFCom1.NotasFiscais.Assinar;
  ACBrNFCom1.NotasFiscais.Validar;

//  ACBrNFCom1.NotasFiscais.Items[0].GravarXML();

  ShowMessage('Arquivo gerado em: ' + ACBrNFCom1.NotasFiscais.Items[0].NomeArq);
  memoLog.Lines.Add('Arquivo gerado em: ' + ACBrNFCom1.NotasFiscais.Items[0].NomeArq);

  ACBrNFCom1.NotasFiscais.LoadFromFile(ACBrNFCom1.NotasFiscais.Items[0].NomeArq);
  Xml := ACBrNFCom1.NotasFiscais.Items[0].XMLAssinado;

  LoadXML(Xml, wbXmlRetorno);

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFCom.btnImprimirDANFComClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName,False);
    ACBrNFCom1.NotasFiscais.Imprimir;
  end;
end;

procedure TfrmACBrNFCom.btnImprimirDANFComOfflineClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName,False);
    ACBrNFCom1.NotasFiscais.Imprimir;
  end;
end;

procedure TfrmACBrNFCom.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.EventoNFCom.Evento.Clear;
    ACBrNFCom1.EventoNFCom.LerXML(OpenDialog1.FileName);
    ACBrNFCom1.ImprimirEvento;
  end;
end;

procedure TfrmACBrNFCom.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrNFCom1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrNFCom1.SSL.CertCertificadora);
end;

procedure TfrmACBrNFCom.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrNFCom1.SSL do
  begin
     CarregarCertificadoPublico(memoLog.Lines.Text);
     memoLog.Lines.Add(CertIssuerName);
     memoLog.Lines.Add(CertRazaoSocial);
     memoLog.Lines.Add(CertCNPJ);
     memoLog.Lines.Add(CertSubjectName);
     memoLog.Lines.Add(CertNumeroSerie);

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

procedure TfrmACBrNFCom.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrNFCom1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrNFCom.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrNFCom.btnStatusServClick(Sender: TObject);
begin
  ACBrNFCom1.WebServices.StatusServico.Executar;

  LoadXML(ACBrNFCom1.WebServices.StatusServico.RetWS, wbXmlRetorno);

  pgRespostas.ActivePageIndex := 0;

  memoLog.Lines.Add('');
  memoLog.Lines.Add('Status Serviço');
  memoLog.Lines.Add('tpAmb: '    +TipoAmbienteToStr(ACBrNFCom1.WebServices.StatusServico.tpAmb));
  memoLog.Lines.Add('verAplic: ' +ACBrNFCom1.WebServices.StatusServico.verAplic);
  memoLog.Lines.Add('cStat: '    +IntToStr(ACBrNFCom1.WebServices.StatusServico.cStat));
  memoLog.Lines.Add('xMotivo: '  +ACBrNFCom1.WebServices.StatusServico.xMotivo);
  memoLog.Lines.Add('cUF: '      +IntToStr(ACBrNFCom1.WebServices.StatusServico.cUF));
  memoLog.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrNFCom1.WebServices.StatusServico.dhRecbto));
  memoLog.Lines.Add('tMed: '     +IntToStr(ACBrNFCom1.WebServices.StatusServico.TMed));
  memoLog.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrNFCom1.WebServices.StatusServico.dhRetorno));
  memoLog.Lines.Add('xObs: '     +ACBrNFCom1.WebServices.StatusServico.xObs);
end;

procedure TfrmACBrNFCom.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrNFCom1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrNFCom1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrNFCom.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg: String;
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    pgRespostas.ActivePageIndex := 0;
    memoLog.Lines.Add('');
    memoLog.Lines.Add('');

    if not ACBrNFCom1.NotasFiscais.VerificarAssinatura(Msg) then
      memoLog.Lines.Add('Erro: '+Msg)
    else
    begin
      memoLog.Lines.Add('OK: Assinatura Válida');
      ACBrNFCom1.SSL.CarregarCertificadoPublico( ACBrNFCom1.NotasFiscais[0].NFCom.signature.X509Certificate );
      memoLog.Lines.Add('Assinado por: '+ ACBrNFCom1.SSL.CertRazaoSocial);
      memoLog.Lines.Add('CNPJ: '+ ACBrNFCom1.SSL.CertCNPJ);
      memoLog.Lines.Add('Num.Série: '+ ACBrNFCom1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmACBrNFCom.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg, Tempo: String;
  Inicio: TDateTime;
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    Inicio := Now;
    Ok := ACBrNFCom1.NotasFiscais.ValidarRegrasdeNegocios(Msg);
    Tempo := FormatDateTime('hh:nn:ss:zzz', Now - Inicio);

    if not Ok then
    begin
      memoLog.Lines.Add('Erro: ' + Msg);
      ShowMessage('Erros encontrados' + sLineBreak + 'Tempo: ' + Tempo);
    end
    else
      ShowMessage('Tudo OK' + sLineBreak + 'Tempo: ' + Tempo);
  end;
end;

procedure TfrmACBrNFCom.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFCom';
  OpenDialog1.DefaultExt := '*-NFCom.XML';
  OpenDialog1.Filter := 'Arquivos NFCom (*-NFCom.XML)|*-NFCom.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrNFCom1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrNFCom1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
  begin
    ACBrNFCom1.NotasFiscais.Clear;
    ACBrNFCom1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    try
      ACBrNFCom1.NotasFiscais.Validar;

      if ACBrNFCom1.NotasFiscais.Items[0].Alertas <> '' then
        memoLog.Lines.Add('Alertas: ' + ACBrNFCom1.NotasFiscais.Items[0].Alertas);

      ShowMessage('Nota Fiscal de Comunicação Valida');
    except
      on E: Exception do
      begin
//        pgRespostas.ActivePage := Dados;
        memoLog.Lines.Add('Exception: ' + E.Message);
        memoLog.Lines.Add('Erro: ' + ACBrNFCom1.NotasFiscais.Items[0].ErroValidacao);
        memoLog.Lines.Add('Erro Completo: ' + ACBrNFCom1.NotasFiscais.Items[0].ErroValidacaoCompleto);
      end;
    end;
  end;
end;

procedure TfrmACBrNFCom.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFCom1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFCom.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFCom1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFCom.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFCom1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFCom.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrNFCom1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrNFCom.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFCom1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFCom.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoNFCom;
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
  for K := Low(TVersaoNFCom) to High(TVersaoNFCom) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoNFCom), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFCom.GravarConfiguracao;
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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathNFCom',  cbxEmissaoPathNFCom.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathNFCom',         edtPathNFCom.Text);
    Ini.WriteString('Arquivos', 'PathEvento',       edtPathEvento.Text);

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

    Ini.WriteInteger('DANFCom', 'Tipo',       rgTipoDANFCom.ItemIndex);
    Ini.WriteString( 'DANFCom', 'LogoMarca',  edtLogoMarca.Text);

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFCom.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrNFCom.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrNFCom.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrNFCom.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrNFCom.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrNFCom.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrNFCom.LerConfiguracao;
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
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoNFCom), integer(cbVersaoDF.ItemIndex) ));

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
    cbxEmissaoPathNFCom.Checked  := Ini.ReadBool(  'Arquivos', 'EmissaoPathNFCom',  false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathNFCom.Text            := Ini.ReadString('Arquivos', 'PathNFCom',         '');
    edtPathEvento.Text          := Ini.ReadString('Arquivos', 'PathEvento',       '');

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

    rgTipoDaNFCom.ItemIndex := Ini.ReadInteger('DANFCom', 'Tipo',       0);
    edtLogoMarca.Text      := Ini.ReadString( 'DANFCom', 'LogoMarca',  '');

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFCom.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrNFCom1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrNFCom1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrNFCom1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;
  {
  if cbModeloDF.ItemIndex = 0 then
    ACBrNFCom1.DANFCom := ACBrNFComDANFComRL1
  else
    ACBrNFCom1.DANFCom := ACBrNFComDANFComESCPOS1;
  }
  ACBrNFCom1.SSL.DescarregarCertificado;

  with ACBrNFCom1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoNFCom(cbVersaoDF.ItemIndex);
  end;

  with ACBrNFCom1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBrNFCom1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrNFCom1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrNFCom1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathNFCom  := cbxEmissaoPathNFCom.Checked;
    SalvarEvento     := cbxSalvaPathEvento.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathNFCom         := edtPathNFCom.Text;
    PathEvento       := edtPathEvento.Text;
    PathMensal       := GetPathNFCom(0);
    PathSalvar       := PathMensal;
 end;

  if ACBrNFCom1.DANFCom <> nil then
  begin
    ACBrNFCom1.DANFCom.TipoDANFCom := StrToTpImp(OK, IntToStr(rgTipoDaNFCom.ItemIndex + 1));
    ACBrNFCom1.DANFCom.Logo       := edtLogoMarca.Text;
  end;
end;

procedure TfrmACBrNFCom.ConfigurarEmail;
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
  ACBrMail1.FromName := 'Projeto ACBr - ACBrNFCom';
end;

procedure TfrmACBrNFCom.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrNFCom1.NotasFiscais.Count > 0 then
    memoLog.Lines.Add('Empresa: ' + ACBrNFCom1.NotasFiscais.Items[0].NFCom.Emit.xNome);
end;

procedure TfrmACBrNFCom.PathClick(Sender: TObject);
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

procedure TfrmACBrNFCom.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrNFCom.sbPathNFComClick(Sender: TObject);
begin
  PathClick(edtPathNFCom);
end;

procedure TfrmACBrNFCom.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFCom.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrNFCom1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrNFCom.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFCom.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  ACBrNFCom1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrNFCom1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrNFCom1.SSL.ListaCertificados[I] do
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

procedure TfrmACBrNFCom.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrNFCom.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TfrmACBrNFCom.ACBrNFCom1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
  Tratado := True;
end;

procedure TfrmACBrNFCom.ACBrNFCom1StatusChange(Sender: TObject);
begin
  case ACBrNFCom1.Status of
    stNFComIdle:
      begin
        if ( frmStatus <> nil ) then
          frmStatus.Hide;
      end;

    stNFComStatusServico:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFComRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados da NFCom...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFComRetRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Recebendo dados da NFCom...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFComConsulta:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando NFCom...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFComRecibo:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando Recibo de Lote...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFComEmail:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFComEvento:
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

end.
