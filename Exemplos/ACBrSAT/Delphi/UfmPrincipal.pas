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

unit UfmPrincipal;

{$I ACBr.inc}

interface

//descomentar o motor de relatório que desejar utilizar! removendo o ponto
{$DEFINE GERADOR_FORTES_REPORT}
{.$DEFINE GERADOR_FAST_REPORT}

uses
  Classes, SysUtils, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ActnList, Menus, ExtCtrls, Buttons,
  ComCtrls, Spin, RLPDFFilter, ACBrSAT, ACBrSATClass, ACBrSATExtratoESCPOS,
  dateutils, ACBrBase, ACBrPosPrinter, ACBrDFeSSL,
  RLFilters, ACBrSATExtratoReportClass, ACBrSATExtratoClass, OleCtrls,
  SHDocVw, ACBrIntegrador, ACBrDFeReport
  {$IFDEF GERADOR_FORTES_REPORT},ACBrSATExtratoFortesFr {$ENDIF}
  {$IFDEF GERADOR_FAST_REPORT},ACBrSATExtratoFR {$ENDIF}
  , ACBrSATExtratoFPDF;

const
  cAssinatura = '9d4c4eef8c515e2c1269c2e4fff0719d526c5096422bf1defa20df50ba06469'+
                'a28adb25ba0447befbced7c0f805a5cc58496b7b23497af9a04f69c77f17c0c'+
                'e68161f8e4ca7e3a94c827b6c563ca6f47aea05fa90a8ce3e4327853bb2d664'+
                'ba226728fff1e2c6275ecc9b20129e1c1d2671a837aa1d265b36809501b519d'+
                'bc08129e1c1d2671a837aa1d265b36809501b519dbc08129e1c1d2671a837aa'+
                '1d265b36809501b519dbc08129e1c';
type

  { TForm1 }

  TfmPrincipal = class(TForm)
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrSAT1 : TACBrSAT;
    ACBrSATExtratoESCPOS1 : TACBrSATExtratoESCPOS;
    bImpressora: TButton;
    bInicializar : TButton;
    btLerParams: TButton;
    btMFEEnviarStatusPagamento: TButton;
    btMFEVerificarStatus: TButton;
    btMFERespostaFiscal: TButton;
    btSalvarParams: TButton;
    btSerial: TSpeedButton;
    btMFEEnviarPagamento: TButton;
    cbxRemoverAcentos: TCheckBox;
    cbxModeloPosPrinter: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbxRedeProxy: TComboBox;
    cbxModelo : TComboBox;
    cbxAmbiente : TComboBox;
    cbxIndRatISSQN : TComboBox;
    cbxRegTribISSQN : TComboBox;
    cbxRegTributario : TComboBox;
    cbxSalvarCFe: TCheckBox;
    cbxSalvarCFeCanc: TCheckBox;
    cbxSalvarEnvio: TCheckBox;
    cbxSepararPorModelo: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    cbxSepararPorAno: TCheckBox;
    cbxSepararPorDia: TCheckBox;
    cbxSepararPorMes: TCheckBox;
    cbxFormatXML: TCheckBox;
    cbPreview: TCheckBox;
    cbxRedeSeg: TComboBox;
    cbxUTF8: TCheckBox;
    cbxXmlSignLib: TComboBox;
    edChaveCancelamento: TEdit;
    edMFEInput: TEdit;
    edMFEOutput: TEdit;
    edLog : TEdit;
    edSchemaVendaAPL: TEdit;
    edRedeIP: TEdit;
    edRedeProxyPorta: TSpinEdit;
    edRedeProxyUser: TEdit;
    edRedeProxySenha: TEdit;
    edRedeMask: TEdit;
    edRedeGW: TEdit;
    edRedeDNS1: TEdit;
    edRedeDNS2: TEdit;
    edRedeUsuario: TEdit;
    edRedeSenha: TEdit;
    edRedeProxyIP: TEdit;
    edRedeSSID: TEdit;
    edRedeCodigo: TEdit;
    edSchemaVendaSAT: TEdit;
    GroupBox2: TGroupBox;
    grpFortesFast: TGroupBox;
    gbESCPOS: TGroupBox;
    gbWiFi: TGroupBox;
    gbIPFix: TGroupBox;
    gbPPPoE: TGroupBox;
    gbProxy: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lImpressora: TLabel;
    lSSID: TLabel;
    lSSID1: TLabel;
    lSSID10: TLabel;
    lSSID11: TLabel;
    lSSID12: TLabel;
    lSSID2: TLabel;
    lSSID3: TLabel;
    lSSID4: TLabel;
    lSSID5: TLabel;
    lSSID6: TLabel;
    lSSID7: TLabel;
    lSSID8: TLabel;
    lSSID9: TLabel;
    mCancelamentoEnviar: TMemo;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    mRede: TWebBrowser;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem9: TMenuItem;
    miGerarXMLCancelamento: TMenuItem;
    miEnviarCancelamento: TMenuItem;
    MenuItem12: TMenuItem;
    miImprimirExtratoCancelamento: TMenuItem;
    Panel2: TPanel;
    PrintDialog1: TPrintDialog;
    rgRedeTipoInter: TRadioGroup;
    rgRedeTipoLan: TRadioGroup;
    RLPDFFilter1: TRLPDFFilter;
    SaveDialog1: TSaveDialog;
    sbNomeDLL: TSpeedButton;
    sbSchemaVendaAPL: TSpeedButton;
    sbSchemaVendaSAT: TSpeedButton;
    seColunas: TSpinEdit;
    seEspLinhas: TSpinEdit;
    seLargura: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seMargemDireita: TSpinEdit;
    seMargemEsquerda: TSpinEdit;
    seMargemFundo: TSpinEdit;
    seMargemTopo: TSpinEdit;
    sePagCod: TSpinEdit;
    Label13: TLabel;
    Label17 : TLabel;
    mLimpar : TMenuItem;
    mImprimirExtratoVendaResumido : TMenuItem;
    mImprimirExtratoVenda : TMenuItem;
    seNumeroCaixa : TSpinEdit;
    edNomeDLL : TEdit;
    edtEmitCNPJ : TEdit;
    edtEmitIE : TEdit;
    edtEmitIM : TEdit;
    edtSwHAssinatura : TEdit;
    edtSwHCNPJ : TEdit;
    edtCodigoAtivacao : TEdit;
    edtCodUF : TEdit;
    GroupBox1 : TGroupBox;
    gpOperacao : TGroupBox;
    Label1 : TLabel;
    Label10 : TLabel;
    Label11 : TLabel;
    Label12 : TLabel;
    Label14 : TLabel;
    Label15 : TLabel;
    Label16 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label9 : TLabel;
    MainMenu1 : TMainMenu;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    mAtivarSAT : TMenuItem;
    mComunicarCertificado : TMenuItem;
    mAssociarAssinatura : TMenuItem;
    mBloquearSAT : TMenuItem;
    MenuItem3 : TMenuItem;
    mDesbloquearSAT : TMenuItem;
    MenuItem4 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem6 : TMenuItem;
    mConsultarStatusOperacional : TMenuItem;
    mConsultarSAT : TMenuItem;
    mConsultarNumeroSessao : TMenuItem;
    MenuItem7 : TMenuItem;
    MenuItem8 : TMenuItem;
    mAtaulizarSoftwareSAT : TMenuItem;
    mConfigurarInterfaceRede : TMenuItem;
    mExtrairLogs : TMenuItem;
    mLog : TMemo;
    mTesteFimAFim : TMenuItem;
    mEnviarVenda : TMenuItem;
    mGerarVenda : TMenuItem;
    OpenDialog1 : TOpenDialog;
    PageControl1 : TPageControl;
    PageControl2 : TPageControl;
    Panel1 : TPanel;
    SbArqLog : TSpeedButton;
    seItensVenda: TSpinEdit;
    seMFETimeout: TSpinEdit;
    Splitter1 : TSplitter;
    StatusBar1 : TStatusBar;
    mVendaEnviar: TMemo;
    mRecebido: TWebBrowser;
    Impressao: TTabSheet;
    TabSheet1: TTabSheet;
    tsMFe: TTabSheet;
    tsRedeXML: TTabSheet;
    tsRede: TTabSheet;
    tsCancelamento: TTabSheet;
    tsDadosEmit : TTabSheet;
    tsDadosSAT : TTabSheet;
    tsDadosSwHouse : TTabSheet;
    tsRecebido : TTabSheet;
    tsLog : TTabSheet;
    tsGerado : TTabSheet;
    sfeVersaoEnt: TEdit;
    cbLogoLateral: TCheckBox;
    cbQRCodeLateral: TCheckBox;
    cbImprimir1Linha: TCheckBox;
    cbImprimirDescAcres: TCheckBox;
    cbImprimirChaveUmaLinha: TCheckBox;
    ACBrIntegrador1: TACBrIntegrador;
    N1: TMenuItem;
    CarregarLote1: TMenuItem;
    ConsultarltimaSessoFiscal1: TMenuItem;
    Label80: TLabel;
    cbxMotorRelatorio: TComboBox;
    Label83: TLabel;
    sbFR3: TSpeedButton;
    edtPathFR3: TEdit;
    Label36: TLabel;

    procedure ACBrSAT1CalcPath(var APath: String; ACNPJ: String;
      AData: TDateTime);
    procedure ACBrSAT1GravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrSAT1MensagemSEFAZ(ACod: Integer; AMensagem: String);
    procedure bImpressoraClick(Sender: TObject);
    procedure bInicializarClick(Sender : TObject);
    procedure btLerParamsClick(Sender : TObject);
    procedure btMFEEnviarStatusPagamentoClick(Sender: TObject);
    procedure btMFEEnviarPagamentoClick(Sender: TObject);
    procedure btMFERespostaFiscalClick(Sender: TObject);
    procedure btMFEVerificarStatusClick(Sender: TObject);
    procedure btSalvarParamsClick(Sender : TObject);
    procedure btSerialClick(Sender: TObject);
    procedure cbxFormatXMLChange(Sender: TObject);
    procedure cbxModeloChange(Sender : TObject);
    procedure cbxRedeProxyChange(Sender: TObject);
    procedure cbxRemoverAcentosChange(Sender: TObject);
    procedure cbxSalvarCFeCancChange(Sender: TObject);
    procedure cbxSalvarCFeChange(Sender: TObject);
    procedure cbxSalvarEnvioChange(Sender: TObject);
    procedure cbxSepararPorAnoChange(Sender: TObject);
    procedure cbxSepararPorCNPJChange(Sender: TObject);
    procedure cbxSepararPorDiaChange(Sender: TObject);
    procedure cbxSepararPorMesChange(Sender: TObject);
    procedure cbxSepararPorModeloChange(Sender: TObject);
    procedure cbxUTF8Change(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure miGerarXMLCancelamentoClick(Sender: TObject);
    procedure miEnviarCancelamentoClick(Sender: TObject);
    procedure miImprimirExtratoCancelamentoClick(Sender: TObject);
    procedure mTesteFimAFimClick(Sender: TObject);
    procedure rgRedeTipoInterClick(Sender: TObject);
    procedure rgRedeTipoLanClick(Sender: TObject);
    procedure sbNomeDLLClick(Sender: TObject);
    procedure sbSchemaVendaAPLClick(Sender: TObject);
    procedure sbSchemaVendaSATClick(Sender: TObject);
    procedure FormCreate(Sender : TObject);
    procedure mAssociarAssinaturaClick(Sender : TObject);
    procedure mAtaulizarSoftwareSATClick(Sender : TObject);
    procedure mAtivarSATClick(Sender : TObject);
    procedure mBloquearSATClick(Sender : TObject);
    procedure mComunicarCertificadoClick(Sender : TObject);
    procedure mConfigurarInterfaceRedeClick(Sender : TObject);
    procedure mConsultarNumeroSessaoClick(Sender : TObject);
    procedure mConsultarSATClick(Sender : TObject);
    procedure mConsultarStatusOperacionalClick(Sender : TObject);
    procedure mDesbloquearSATClick(Sender : TObject);
    procedure MenuItem5Click(Sender : TObject);
    procedure mEnviarVendaClick(Sender : TObject);
    procedure mExtrairLogsClick(Sender : TObject);
    procedure mGerarVendaClick(Sender : TObject);
    procedure mImprimirExtratoVendaClick(Sender : TObject);
    procedure mImprimirExtratoVendaResumidoClick(Sender : TObject);
    procedure mLimparClick(Sender : TObject);
    procedure SbArqLogClick(Sender : TObject);
    procedure sePagCodChange(Sender: TObject);
    procedure sfeVersaoEntChange(Sender: TObject);
    procedure CarregarLote1Click(Sender: TObject);
    procedure ConsultarltimaSessoFiscal1Click(Sender: TObject);
    procedure cbxMotorRelatorioChange(Sender: TObject);
    procedure sbFR3Click(Sender: TObject);
  private
    FACBrSATExtratoFPDF1: TACBrSATExtratoFPDF;

    {$IFDEF GERADOR_FORTES_REPORT}
      FACBrSATExtratoFortes1: TACBrSATExtratoFortes;
    {$ENDIF}
    {$IFDEF GERADOR_FAST_REPORT}
      FACBrSATExtratoFR1: TACBrSATExtratoFR;
    {$ENDIF}

    procedure AdicionaModelosExtratoNoComboBox;
    procedure ConfiguraRedeSAT;
    procedure LeDadosRedeSAT;
    procedure PrepararImpressao;
    procedure TrataErros(Sender : TObject; E : Exception);
    procedure AjustaACBrSAT;
    procedure LoadXML(const AXML: String; MyWebBrowser: TWebBrowser);

    procedure GetsignAC(var Chave: AnsiString);
    procedure GetcodigoDeAtivacao(var Chave: AnsiString);
  public
  end;

var
  fmPrincipal : TfmPrincipal;

implementation

Uses
  math, typinfo, pcnConversao, pcnRede, synacode, IniFiles, configuraserial,
  RLPrinters, Printers, ACBrSATMFe_integrador, pcnVFPe,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.FilesIO;

{$R *.dfm}

{ TForm1 }

procedure TfmPrincipal.FormCreate(Sender : TObject);
var
  I: TACBrSATModelo;
  J: TpcnTipoAmbiente;
  K: TpcnRegTribISSQN;
  L: TpcnindRatISSQN;
  M: TpcnRegTrib;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
  R: pcnRede.TSegSemFio;
  P: TSSLXmlSignLib;
begin
  ACBrSAT1.OnGetsignAC := GetsignAC;
  ACBrSAT1.OnGetcodigoDeAtivacao := GetcodigoDeAtivacao;

  cbxModelo.Items.Clear;
  For I := Low(TACBrSATModelo) to High(TACBrSATModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrSATModelo), integer(I) ) );

  cbxRedeSeg.Items.Clear;
  For R := Low(pcnRede.TSegSemFio) to High(pcnRede.TSegSemFio) do
     cbxRedeSeg.Items.Add(GetEnumName(TypeInfo(pcnRede.TSegSemFio), Integer(R)));

  cbxAmbiente.Items.Clear;
  For J := Low(TpcnTipoAmbiente) to High(TpcnTipoAmbiente) do
     cbxAmbiente.Items.Add( GetEnumName(TypeInfo(TpcnTipoAmbiente), integer(J) ) );

  cbxRegTribISSQN.Items.Clear;
  For K := Low(TpcnRegTribISSQN) to High(TpcnRegTribISSQN) do
     cbxRegTribISSQN.Items.Add( GetEnumName(TypeInfo(TpcnRegTribISSQN), integer(K) ) );

  cbxIndRatISSQN.Items.Clear;
  For L := Low(TpcnindRatISSQN) to High(TpcnindRatISSQN) do
     cbxIndRatISSQN.Items.Add( GetEnumName(TypeInfo(TpcnindRatISSQN), integer(L) ) );

  cbxRegTributario.Items.Clear;
  For M := Low(TpcnRegTrib) to High(TpcnRegTrib) do
     cbxRegTributario.Items.Add( GetEnumName(TypeInfo(TpcnRegTrib), integer(M) ) );

  cbxModeloPosPrinter.Items.Clear;
  For N := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModeloPosPrinter.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(N) ) );

  cbxPagCodigo.Items.Clear;
  For O := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(O) ) );

  cbxXmlSignLib.Items.Clear;
  For P := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
     cbxXmlSignLib.Items.Add( GetEnumName(TypeInfo(TSSLXmlSignLib), integer(P) ) );

  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  ACBrPosPrinter1.Device.AcharPortasRAW( cbxPorta.Items );
  {$IfDef MSWINDOWS}
  ACBrPosPrinter1.Device.AcharPortasUSB( cbxPorta.Items );
  {$EndIf}

  cbxPorta.Items.Add('\\localhost\Epson') ;
  cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;

  {$IfNDef MSWINDOWS}
   cbxPorta.Items.Add('/dev/ttyS0') ;
   cbxPorta.Items.Add('/dev/ttyS1') ;
   cbxPorta.Items.Add('/dev/ttyUSB0') ;
   cbxPorta.Items.Add('/dev/ttyUSB1') ;
   cbxPorta.Items.Add('/tmp/ecf.txt') ;
  {$EndIf}

  Application.OnException := TrataErros;

  PageControl1.ActivePageIndex := 0;
  PageControl2.ActivePageIndex := 0;

  AdicionaModelosExtratoNoComboBox;

  btLerParams.Click;

end;

procedure TfmPrincipal.mAssociarAssinaturaClick(Sender : TObject);
begin
  ACBrSAT1.AssociarAssinatura( edtSwHCNPJ.Text + edtEmitCNPJ.Text, edtSwHAssinatura.Text );
end;

procedure TfmPrincipal.mAtaulizarSoftwareSATClick(Sender : TObject);
begin
  ACBrSAT1.AtualizarSoftwareSAT;
end;

procedure TfmPrincipal.TrataErros(Sender: TObject; E: Exception);
var
  Erro: String;
begin
  Erro := Trim(E.Message);
  ACBrSAT1.DoLog( E.ClassName+' - '+Erro);
end;

procedure TfmPrincipal.AjustaACBrSAT;
begin
  with ACBrSAT1 do
  begin
    Modelo  := TACBrSATModelo( cbxModelo.ItemIndex );
    ArqLOG  := edLog.Text;
    NomeDLL := edNomeDLL.Text;
    Config.ide_numeroCaixa := seNumeroCaixa.Value;
    Config.ide_tpAmb       := TpcnTipoAmbiente( cbxAmbiente.ItemIndex );
    Config.ide_CNPJ        := edtSwHCNPJ.Text;
    Config.emit_CNPJ       := edtEmitCNPJ.Text;
    Config.emit_IE         := edtEmitIE.Text;
    Config.emit_IM         := edtEmitIM.Text;
    Config.emit_cRegTrib      := TpcnRegTrib( cbxRegTributario.ItemIndex );
    Config.emit_cRegTribISSQN := TpcnRegTribISSQN( cbxRegTribISSQN.ItemIndex );
    Config.emit_indRatISSQN   := TpcnindRatISSQN( cbxIndRatISSQN.ItemIndex );
    Config.PaginaDeCodigo     := sePagCod.Value;
    Config.EhUTF8             := cbxUTF8.Checked;
    Config.infCFe_versaoDadosEnt := StringToFloatDef( sfeVersaoEnt.Text, cversaoDadosEnt);

    ConfigArquivos.SalvarCFe := cbxSalvarCFe.Checked;
    ConfigArquivos.SalvarCFeCanc := cbxSalvarCFeCanc.Checked;
    ConfigArquivos.SalvarEnvio := cbxSalvarEnvio.Checked;
    ConfigArquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
    ConfigArquivos.SepararPorModelo := cbxSepararPorModelo.Checked;
    ConfigArquivos.SepararPorDia := cbxSepararPorDia.Checked;
    ConfigArquivos.SepararPorMes := cbxSepararPorMes.Checked;
    ConfigArquivos.SepararPorAno := cbxSepararPorAno.Checked;

    if Modelo = mfe_Integrador_XML then
    begin
      ACBrIntegrador1.PastaInput  := edMFEInput.Text;
      ACBrIntegrador1.PastaOutput := edMFEOutput.Text;
      ACBrIntegrador1.Timeout     := seMFETimeout.Value;

      Integrador := ACBrIntegrador1;
    end;
  end
end;

procedure TfmPrincipal.ACBrSAT1GravarLog(const ALogLine: String; var Tratado: Boolean);
begin
  mLog.Lines.Add(ALogLine);
  StatusBar1.Panels[0].Text := IntToStr( ACBrSAT1.Resposta.numeroSessao );
  StatusBar1.Panels[1].Text := IntToStr( ACBrSAT1.Resposta.codigoDeRetorno );
  Tratado := False;
end;

procedure TfmPrincipal.ACBrSAT1MensagemSEFAZ(ACod: Integer; AMensagem: String);
begin
  MessageDlg( IntToStr(ACod)+'-'+AMensagem, mtWarning, [mbOK], 0);
end;

procedure TfmPrincipal.ACBrSAT1CalcPath(var APath: String; ACNPJ: String;
  AData: TDateTime);
begin
  mLog.Lines.Add('O Path para o CNPJ: '+ACNPJ+' Data: '+DateToStr(AData)+' é: '+APath);
end;

procedure TfmPrincipal.AdicionaModelosExtratoNoComboBox;
begin
  {$IFDEF GERADOR_FORTES_REPORT}
    FACBrSATExtratoFortes1   := TACBrSATExtratoFortes.Create(ACBrSAT1);
    cbxMotorRelatorio.AddItem('Fortes Reports', FACBrSATExtratoFortes1);
  {$ENDIF}

  {$IFDEF GERADOR_FAST_REPORT}
    FACBrSATExtratoFR1 := TACBrSATExtratoFR.Create(ACBrSAT1);
    cbxMotorRelatorio.AddItem('Fast Reports', FACBrSATExtratoFR1);
  {$ENDIF}

  cbxMotorRelatorio.AddItem('ESCPOS', ACBrSATExtratoESCPOS1);

  FACBrSATExtratoFPDF1 := TACBrSATExtratoFPDF.Create(ACBrSAT1);
  cbxMotorRelatorio.AddItem('FPDF', FACBrSATExtratoFPDF1);
end;

procedure TfmPrincipal.bImpressoraClick(Sender: TObject);
begin
  if PrintDialog1.Execute then
    lImpressora.Caption := Printer.Printers[Printer.PrinterIndex];
end;

procedure TfmPrincipal.bInicializarClick(Sender : TObject);
begin
  AjustaACBrSAT;

  ACBrSAT1.Inicializado := not ACBrSAT1.Inicializado;

  if ACBrSAT1.Inicializado then
    bInicializar.Caption := 'DesInicializar'
  else
    bInicializar.Caption := 'Inicializar';
end;

procedure TfmPrincipal.btLerParamsClick(Sender : TObject);
var
  ArqINI: String;
  INI : TIniFile;
begin
  ArqINI := ChangeFileExt( Application.ExeName,'.ini' );

  INI := TIniFile.Create(ArqINI);
  try
    cbxModelo.ItemIndex    := INI.ReadInteger('SAT','Modelo',0);
    edLog.Text             := INI.ReadString('SAT','ArqLog','ACBrSAT.log');
    edNomeDLL.Text         := INI.ReadString('SAT','NomeDLL','C:\SAT\SAT.DLL');
    edtCodigoAtivacao.Text := INI.ReadString('SAT','CodigoAtivacao','123456');
    edtCodUF.Text          := INI.ReadString('SAT','CodigoUF','35');
    seNumeroCaixa.Value    := INI.ReadInteger('SAT','NumeroCaixa',1);
    cbxAmbiente.ItemIndex  := INI.ReadInteger('SAT','Ambiente',1);
    sePagCod.Value         := INI.ReadInteger('SAT','PaginaDeCodigo',0);
    sfeVersaoEnt.Text      := FloatToString( INI.ReadFloat('SAT','versaoDadosEnt', cversaoDadosEnt) );
    cbxFormatXML.Checked   := INI.ReadBool('SAT','FormatarXML', True);
    cbxUTF8.Checked        := INI.ReadBool('SAT','UTF8', False);
    cbxRemoverAcentos.Checked:= INI.ReadBool('SAT','RetirarAcentos', True);
    cbxSalvarCFe.Checked     := INI.ReadBool('SAT','SalvarCFe', True);
    cbxSalvarCFeCanc.Checked := INI.ReadBool('SAT','SalvarCFeCanc', True);
    cbxSalvarEnvio.Checked   := INI.ReadBool('SAT','SalvarEnvio', True);
    cbxSepararPorCNPJ.Checked:= INI.ReadBool('SAT','SepararPorCNPJ', True);
    cbxSepararPorModelo.Checked := INI.ReadBool('SAT','SepararPorModelo', True);
    cbxSepararPorDia.Checked := INI.ReadBool('SAT','SepararPorDIA', True);
    cbxSepararPorMes.Checked := INI.ReadBool('SAT','SepararPorMES', True);
    cbxSepararPorAno.Checked := INI.ReadBool('SAT','SepararPorANO', True);
    edSchemaVendaAPL.Text := INI.ReadString('SAT','SchemaVendaAPL','');
    edSchemaVendaSAT.Text := INI.ReadString('SAT','SchemaVendaSAT','');
    cbxXmlSignLib.ItemIndex := INI.ReadInteger('SAT','XMLLib',Integer(ACBrSAT1.Config.XmlSignLib));
    sePagCodChange(Sender);

    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger('PosPrinter', 'Modelo', Integer(ACBrPosPrinter1.Modelo));
    cbxPorta.Text := INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex := INI.ReadInteger('PosPrinter','PaginaDeCodigo',Integer(ACBrPosPrinter1.PaginaDeCodigo));
    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter','ParamsString','');
    seColunas.Value := INI.ReadInteger('PosPrinter','Colunas',ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasEntreCupons',ACBrPosPrinter1.LinhasEntreCupons);

    edtEmitCNPJ.Text := INI.ReadString('Emit','CNPJ','');
    edtEmitIE.Text   := INI.ReadString('Emit','IE','');
    edtEmitIM.Text   := INI.ReadString('Emit','IM','');
    cbxRegTributario.ItemIndex := INI.ReadInteger('Emit','RegTributario',0);
    cbxRegTribISSQN.ItemIndex  := INI.ReadInteger('Emit','RegTribISSQN',0);
    cbxIndRatISSQN.ItemIndex   := INI.ReadInteger('Emit','IndRatISSQN',0);

    edtSwHCNPJ.Text       := INI.ReadString('SwH','CNPJ','11111111111111');
    edtSwHAssinatura.Text := INI.ReadString('SwH','Assinatura',cAssinatura);

    cbxMotorRelatorio.ItemIndex := cbxMotorRelatorio.Items.IndexOf(INI.ReadString('SAT','MotorRelatorio', ''));

    {$IFDEF GERADOR_FORTES_REPORT}
    seLargura.Value        := INI.ReadInteger('Fortes','Largura', FACBrSATExtratoFortes1.LarguraBobina);
    seMargemTopo.Value     := INI.ReadInteger('Fortes','MargemTopo', Trunc(FACBrSATExtratoFortes1.MargemSuperior));
    seMargemFundo.Value    := INI.ReadInteger('Fortes','MargemFundo', Trunc(FACBrSATExtratoFortes1.MargemInferior));
    seMargemEsquerda.Value := INI.ReadInteger('Fortes','MargemEsquerda', Trunc(FACBrSATExtratoFortes1.MargemEsquerda));
    seMargemDireita.Value  := INI.ReadInteger('Fortes','MargemDireita', Trunc(FACBrSATExtratoFortes1.MargemDireita));
    cbPreview.Checked      := INI.ReadBool('Fortes','Preview',True);
    {$ENDIF}

    edtPathFR3.Text := INI.ReadString('Fast','CaminhoFR3',EmptyStr);

    lImpressora.Caption    := INI.ReadString('Printer','Name', '');
    if EstaVazio(lImpressora.Caption) then
    begin
      Printer.PrinterIndex := -1;
      lImpressora.Caption  := Printer.Printers[Printer.PrinterIndex];
    end;

    cbImprimirChaveUmaLinha.Checked := INI.ReadBool('EscPos','ImprimirChaveUmaLinha',cbImprimirChaveUmaLinha.Checked);

    cbImprimir1Linha.Checked := INI.ReadBool('Printer','ImprimirItemUmaLinha',cbImprimir1Linha.Checked);
    cbImprimirDescAcres.Checked := INI.ReadBool('Printer','ImprimeDescAcres',cbImprimirDescAcres.Checked);
    cbLogoLateral.Checked := INI.ReadBool('Printer','LogoLateral',cbLogoLateral.Checked);
    cbQRCodeLateral.Checked := INI.ReadBool('Printer','QRCodeLateral',cbQRCodeLateral.Checked);

    rgRedeTipoInter.ItemIndex := INI.ReadInteger('Rede','tipoInter',0);
    rgRedeTipoLan.ItemIndex   := INI.ReadInteger('Rede','tipoLan',0);
    edRedeSSID.Text           := INI.ReadString('Rede','SSID','');
    cbxRedeSeg.ItemIndex      := INI.ReadInteger('Rede','seg',0);
    edRedeCodigo.Text         := INI.ReadString('Rede','codigo','');
    edRedeIP.Text             := INI.ReadString('Rede','lanIP','');
    edRedeMask.Text           := INI.ReadString('Rede','lanMask','');
    edRedeGW.Text             := INI.ReadString('Rede','lanGW','');
    edRedeDNS1.Text           := INI.ReadString('Rede','lanDNS1','');
    edRedeDNS2.Text           := INI.ReadString('Rede','lanDNS2','');
    edRedeUsuario.Text        := INI.ReadString('Rede','usuario','');
    edRedeSenha.Text          := INI.ReadString('Rede','senha','');
    cbxRedeProxy.ItemIndex    := INI.ReadInteger('Rede','proxy',0);
    edRedeProxyIP.Text        := INI.ReadString('Rede','proxy_ip','');
    edRedeProxyPorta.Value    := INI.ReadInteger('Rede','proxy_porta',0);
    edRedeProxyUser.Text      := INI.ReadString('Rede','proxy_user','');
    edRedeProxySenha.Text     := INI.ReadString('Rede','proxy_senha','');

    edMFEInput.Text    :=  INI.ReadString('MFE','Input','c:\Integrador\Input\');
    edMFEOutput.Text   :=  INI.ReadString('MFE','Output','c:\Integrador\Output\');
    seMFETimeout.Value :=  INI.ReadInteger('MFE','Timeout',30);
  finally
    INI.Free;
  end;

  if edSchemaVendaAPL.Text = '' then
  begin
    if FileExists('CfeDadosVendaAPL_0007.xsd') then
      edSchemaVendaAPL.Text := 'CfeDadosVendaAPL_0007.xsd'
  end;

  if edSchemaVendaSAT.Text = '' then
  begin
    if FileExists('CfeDadosVendaSAT_0007.xsd') then
      edSchemaVendaSAT.Text := 'CfeDadosVendaSAT_0007.xsd'
  end;
end;

//Este método é utilizado para Envio de Pagamento utilizando TEF
procedure TfmPrincipal.btMFEEnviarStatusPagamentoClick(Sender: TObject);
var
  StatusPagamentoMFe : TStatusPagamento;
  RespostaStatusPagamento : TRespostaStatusPagamento;
begin
  {OBS: Para quem utiliza o Componente ACBrTEFD os dados abaixo podem ser obtidos
  utilizando a propriedade ACBrTEFDRespNFCeSAT da classe de retorno TACBrTEFDResp do componente ACBrTEFD  }
  StatusPagamentoMFe := TStatusPagamento.Create;
  try
    with StatusPagamentoMFe do
    begin
      Clear;
      ChaveAcessoValidador := '25CFE38D-3B92-46C0-91CA-CFF751A82D3D';     //Esta chave é Fixa (Esta chave está definida no Manual Integrador Fiscal para Testes)
      CodigoAutorizacao := '20551';                                       //Código de Realização de Pagamento retornado pela Adquirinte (Autorizadora Pagamento)
      Bin := '123456';                                                    //Sequencial de Número Iniciais do Cartão fornecidos pela Adquirinte
      DonoCartao := 'TESTE';                                              //Nome Do titular do Cartão fornecido pela Adquirinte
      DataExpiracao := '01/01';                                           //Data de Validade do Cartão fornecido pela adquirinte
      InstituicaoFinanceira:= 'STONE';                                    //Adquirinte que realizou o Pagamento
      Parcelas := 1;                                                      //Qtd de parcelas aprovadas pela adquirinte
      CodigoPagamento := '12846';                                         //NSU de Pagamento fornecido pela adquirinte
      ValorPagamento := 1530;                                             //Valor Documento fornecido pela adquirinte
      IDFila := 1674068;                                                  //NSU fornecido pela SEFAZ retornado na solicitação de Pagamento
      Tipo := '1';                                                        //Tipo da Bandeira do Cartão Fornecido pela adquirinte
      UltimosQuatroDigitos := 1234;                                       //Ultimos quatro dígitos do cartão fornecidos pela adquirinte
    end;

    if ACBrSAT1.SAT is TACBrSATMFe_integrador_XML then
      RespostaStatusPagamento := TACBrSATMFe_integrador_XML(ACBrSAT1.SAT).EnviarStatusPagamento(StatusPagamentoMFe)  //Retorna Resposta do Integrador com IDPagamento
    else
      RespostaStatusPagamento := ACBrIntegrador1.EnviarStatusPagamento(StatusPagamentoMFe);

    if Assigned(RespostaStatusPagamento) then
    begin
      showMessage(RespostaStatusPagamento.Retorno);
      LoadXML( RespostaStatusPagamento.XML, mRecebido );
      PageControl1.ActivePage := tsRecebido;
    end;
  finally
    StatusPagamentoMFe.Free;
  end;
end;

//Este método é utilizado para Envio de Pagamento utilizando POS
procedure TfmPrincipal.btMFEEnviarPagamentoClick(Sender: TObject);
var
  PagamentoMFe : TEnviarPagamento;
  RespostaPagamentoMFe : TRespostaPagamento;
begin
  RespostaPagamentoMFe := Nil;
  PagamentoMFe := TEnviarPagamento.Create;
  try
    with PagamentoMFe do
    begin
      Clear;
      ChaveAcessoValidador := '25CFE38D-3B92-46C0-91CA-CFF751A82D3D';                          //Esta chave é Fixa, (Esta chave está definida no Manual Integrador Fiscal)
      ChaveRequisicao := '26359854-5698-1365-9856-965478231456';                               //Esta chave deve ser unica por requisição, deve-se gerar um GUID com a combinação: (Cód. Filial / Adquirinte ex:(Cielo) / ID ) - (Esta chave está definida no Manual Integrador Fiscal, para testes)
      Estabelecimento := '10';                                                                 //Codigo do estabelecimento fornecido pela Adquirinte contratada, enquanto não estiver funcionando a integração deve informar valor fixo
      SerialPOS := InputBox('SerialPOS','Informe o Serial do POS','ACBr-'+RandomName(8));      //Como não existe este equipamento POS Integrado conforme previsão inicial da SEFAZ CE, está sendo informado o Numero serial do Equipamento POS, independente do Modelo utilizado. (Está gerando random para diversificar o código neste Demo )
      CNPJ := edtEmitCNPJ.Text;                                                                //CNPJ do Contribuinte
      IcmsBase := 0.23;                                                                        //Valor sujeito a legislação do ICMS
      ValorTotalVenda := 1530;                                                                 //Valor total a ser cobrado
      HabilitarMultiplosPagamentos := True;                                                    //Opção para mais de uma forma de pagamento ou mais de um cartão
      HabilitarControleAntiFraude := False;                                                    //Será usado para validação de possíveis fraudes no pagamento
      CodigoMoeda := 'BRL';                                                                    //Formato da moeda
      EmitirCupomNFCE := False;                                                                //Usado Apenas para emissão de NFCe direto pelo equipamento POS
      OrigemPagamento := 'Mesa 1234';                                                          //Descrição da origem do pagamento
    end;

    if ACBrSAT1.SAT is TACBrSATMFe_integrador_XML then
      RespostaPagamentoMFe := TACBrSATMFe_integrador_XML(ACBrSAT1.SAT).EnviarPagamento(PagamentoMFe)
    else
      RespostaPagamentoMFe := ACBrIntegrador1.EnviarPagamento(PagamentoMFe);

    if Assigned(RespostaPagamentoMFe) then
    begin
      ShowMessage(IntToStr(RespostaPagamentoMFe.IDPagamento));
      LoadXML( RespostaPagamentoMFe.XML, mRecebido );
      PageControl1.ActivePage := tsRecebido;
    end;
  finally
    PagamentoMFe.Free;
    if Assigned(RespostaPagamentoMFe) then
      RespostaPagamentoMFe.Free;
  end;
end;

//Este método é utilizado para Finalizar o processo de Pagamento, vinculando um Pagamento a um Documento Fiscal
//Utilizado também para envio do Extrato CFe para impressão no equipamento POS
procedure TfmPrincipal.btMFERespostaFiscalClick(Sender: TObject);
var
  RespostaFiscal : TRespostaFiscal;
  RetornoRespostaFiscal : TRetornoRespostaFiscal;
Begin
  RespostaFiscal := TRespostaFiscal.Create;
  try
    with RespostaFiscal do
    begin
      Clear;
      ChaveAcessoValidador := '25CFE38D-3B92-46C0-91CA-CFF751A82D3D';                           //Esta chave é Fixa, (Esta chave está definida no Manual Integrador Fiscal para Testes)
      IDFila := StrToIntDef(InputBox('IDPagmento','Informe o ID do Pagamento',''),0);           //Este ID corresponde ao IDPagamento retornado na Solicitação de: EnviarPagamento
      ChaveAcesso := '35170408723218000186599000113100000279731880';                            //Chave do Documento Fiscal (CFe) Autorizado no pagamento
      Nsu := '1674068';                                                                         //NSU de Pagamento fornecido pela Adquirente(Autorizadora Pagamento)
      NumerodeAprovacao := '123456';                                                            //Código de Autorização de Pagamento Realizado, retornado pela Adquirinte (Autorizadora Pagamento)
      Bandeira := 'VISA';                                                                       //Tipo de Bandeira do Cartão fornecido pela Adquirinte
      Adquirente := 'STONE';                                                                    //Adquirinte que realizou a Operação de Aprovação de Pagamento
      if Assigned(ACBrSAT1.CFe) and (ACBrSAT1.Extrato = ACBrSATExtratoESCPOS1) then
        ImpressaoFiscal := '<![CDATA['+ACBrSATExtratoESCPOS1.GerarImpressaoFiscalMFe+']]>';     //Extrato do CFe para impressão no aparelho POS
      NumeroDocumento := '1674068';                                                             //Número do Cupom Fiscal Autorizado
      CNPJ:= edtEmitCNPJ.Text;                                                                  //CNPJ do contribuinte
    end;
    if ACBrSAT1.SAT is TACBrSATMFe_integrador_XML then
      RetornoRespostaFiscal := TACBrSATMFe_integrador_XML(ACBrSAT1.SAT).RespostaFiscal(RespostaFiscal)
    else
      RetornoRespostaFiscal := ACBrIntegrador1.RespostaFiscal(RespostaFiscal);

    if Assigned(RetornoRespostaFiscal) then
    begin
      ShowMessage(RetornoRespostaFiscal.IdRespostaFiscal);
      LoadXML( RetornoRespostaFiscal.XML, mRecebido );
      PageControl1.ActivePage := tsRecebido;
    end;
  finally
    RespostaFiscal.Free;
  end;
end;

//OBS: Esta funcionalidade está disponível apenas para testes em Homologação utilizando o "Simulador POS Ceara"
//Como não existe este equipamento POS Integrado conforme previsão inicial da SEFAZ CE, não deve ser utilizado em Produção
procedure TfmPrincipal.btMFEVerificarStatusClick(Sender: TObject);
var
  VerificarStatusValidador : TVerificarStatusValidador;
  RespostaVerificarStatusValidador : TRespostaVerificarStatusValidador;
begin
  VerificarStatusValidador := TVerificarStatusValidador.Create;
  try
    with VerificarStatusValidador do
    begin
      Clear;
      ChaveAcessoValidador := '25CFE38D-3B92-46C0-91CA-CFF751A82D3D';                          //Esta chave é Fixa, (Esta chave está definida no Manual Integrador Fiscal para Testes)
      IDFila := StrToIntDef(InputBox('IDPagmento','Informe o ID do Pagamento',''),0);          //Deve ser informado o ID de Pagamento retornado pelo Método: EnviarPagamento
      CNPJ:= edtEmitCNPJ.Text;                                                                 //CNPJ do Contribuinte
    end;
    if ACBrSAT1.SAT is TACBrSATMFe_integrador_XML then
      RespostaVerificarStatusValidador := TACBrSATMFe_integrador_XML(ACBrSAT1.SAT).VerificarStatusValidador(VerificarStatusValidador)  //Retorna Resposta do Integrador com IDPagamento
    else
      RespostaVerificarStatusValidador := ACBrIntegrador1.VerificarStatusValidador(VerificarStatusValidador);

    if Assigned(RespostaVerificarStatusValidador) then
    begin
      ShowMessage(RespostaVerificarStatusValidador.CodigoAutorizacao);
      LoadXML( RespostaVerificarStatusValidador.XML, mRecebido );
      PageControl1.ActivePage := tsRecebido;
    end;

  finally
    VerificarStatusValidador.Free;
  end;

  ShowMessage(RespostaVerificarStatusValidador.CodigoAutorizacao);
end;

procedure TfmPrincipal.btSalvarParamsClick(Sender : TObject);
Var
  ArqINI : String;
  INI : TIniFile;
begin
  ArqINI := ChangeFileExt( Application.ExeName,'.ini' );

  INI := TIniFile.Create(ArqINI);
  try
    INI.WriteInteger('SAT','Modelo',cbxModelo.ItemIndex);
    INI.WriteString('SAT','ArqLog',edLog.Text);
    INI.WriteString('SAT','NomeDLL',edNomeDLL.Text);
    INI.WriteString('SAT','CodigoAtivacao',edtCodigoAtivacao.Text);
    INI.WriteString('SAT','CodigoUF',edtCodUF.Text);
    INI.WriteInteger('SAT','NumeroCaixa',seNumeroCaixa.Value);
    INI.WriteInteger('SAT','Ambiente',cbxAmbiente.ItemIndex);
    INI.WriteInteger('SAT','PaginaDeCodigo',sePagCod.Value);
    INI.WriteFloat('SAT','versaoDadosEnt', StringToFloatDef( sfeVersaoEnt.Text, cversaoDadosEnt) );
    INI.WriteBool('SAT','FormatarXML', cbxFormatXML.Checked);
    INI.WriteBool('SAT','UTF8', cbxUTF8.Checked);
    INI.WriteBool('SAT','RetirarAcentos', cbxRemoverAcentos.Checked);
    INI.WriteBool('SAT','SalvarCFe', cbxSalvarCFe.Checked);
    INI.WriteBool('SAT','SalvarCFeCanc', cbxSalvarCFeCanc.Checked);
    INI.WriteBool('SAT','SalvarEnvio', cbxSalvarEnvio.Checked);
    INI.WriteBool('SAT','SepararPorCNPJ', cbxSepararPorCNPJ.Checked);
    INI.WriteBool('SAT','SepararPorModelo', cbxSepararPorModelo.Checked);
    INI.WriteBool('SAT','SepararPorDIA', cbxSepararPorDia.Checked);
    INI.WriteBool('SAT','SepararPorMES', cbxSepararPorMes.Checked);
    INI.WriteBool('SAT','SepararPorANO', cbxSepararPorAno.Checked);
    INI.WriteString('SAT','SchemaVendaAPL',edSchemaVendaAPL.Text);
    INI.WriteString('SAT','SchemaVendaSAT',edSchemaVendaSAT.Text);
    INI.WriteInteger('SAT','XMLLib',cbxXmlSignLib.ItemIndex);
    INI.WriteString('SAT','MotorRelatorio',cbxMotorRelatorio.Text);

    INI.WriteInteger('PosPrinter','Modelo',cbxModeloPosPrinter.ItemIndex);
    INI.WriteString('PosPrinter','Porta',cbxPorta.Text);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
    INI.WriteString('PosPrinter','ParamsString',ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger('PosPrinter','Colunas',seColunas.Value);
    INI.WriteInteger('PosPrinter','EspacoLinhas',seEspLinhas.Value);
    INI.WriteInteger('PosPrinter','LinhasEntreCupons',seLinhasPular.Value);

    INI.WriteString('Emit','CNPJ',edtEmitCNPJ.Text);
    INI.WriteString('Emit','IE',edtEmitIE.Text);
    INI.WriteString('Emit','IM',edtEmitIM.Text);
    INI.WriteInteger('Emit','RegTributario',cbxRegTributario.ItemIndex);
    INI.WriteInteger('Emit','RegTribISSQN',cbxRegTribISSQN.ItemIndex);
    INI.WriteInteger('Emit','IndRatISSQN',cbxIndRatISSQN.ItemIndex);

    INI.WriteString('SwH','CNPJ',edtSwHCNPJ.Text);
    INI.WriteString('SwH','Assinatura',edtSwHAssinatura.Text);

    INI.WriteInteger('Fortes','Largura',seLargura.Value);
    INI.WriteInteger('Fortes','MargemTopo',seMargemTopo.Value);
    INI.WriteInteger('Fortes','MargemFundo',seMargemFundo.Value);
    INI.WriteInteger('Fortes','MargemEsquerda',seMargemEsquerda.Value);
    INI.WriteInteger('Fortes','MargemDireita',seMargemDireita.Value);
    INI.WriteBool('Fortes','Preview',cbPreview.Checked);

    INI.WriteString('Fast','CaminhoFR3', edtPathFR3.Text);

    INI.WriteString('Printer','Name',lImpressora.Caption);
    INI.WriteBool('EscPos','ImprimirChaveUmaLinha',cbImprimirChaveUmaLinha.Checked);

    INI.WriteBool('Printer','ImprimirItemUmaLinha',cbImprimir1Linha.Checked);
    INI.WriteBool('Printer','ImprimeDescAcres',cbImprimirDescAcres.Checked);
    INI.WriteBool('Printer','LogoLateral',cbLogoLateral.Checked);
    INI.WriteBool('Printer','QRCodeLateral',cbQRCodeLateral.Checked);

    INI.WriteInteger('Rede','tipoInter',rgRedeTipoInter.ItemIndex);
    INI.WriteInteger('Rede','tipoLan',rgRedeTipoLan.ItemIndex);
    INI.WriteString('Rede','SSID',edRedeSSID.Text);
    INI.WriteInteger('Rede','seg',cbxRedeSeg.ItemIndex);
    INI.WriteString('Rede','codigo',edRedeCodigo.Text);
    INI.WriteString('Rede','lanIP',edRedeIP.Text);
    INI.WriteString('Rede','lanMask',edRedeMask.Text);
    INI.WriteString('Rede','lanGW',edRedeGW.Text);
    INI.WriteString('Rede','lanDNS1',edRedeDNS1.Text);
    INI.WriteString('Rede','lanDNS2',edRedeDNS2.Text);
    INI.WriteString('Rede','usuario',edRedeUsuario.Text);
    INI.WriteString('Rede','senha',edRedeSenha.Text);
    INI.WriteInteger('Rede','proxy',cbxRedeProxy.ItemIndex);
    INI.WriteString('Rede','proxy_ip',edRedeProxyIP.Text);
    INI.WriteInteger('Rede','proxy_porta',edRedeProxyPorta.Value);
    INI.WriteString('Rede','proxy_user',edRedeProxyUser.Text);
    INI.WriteString('Rede','proxy_senha',edRedeProxySenha.Text);

    INI.WriteString('MFE','Input',edMFEInput.Text);
    INI.WriteString('MFE','Output',edMFEOutput.Text);
    INI.WriteInteger('MFE','Timeout',seMFETimeout.Value);
  finally
    INI.Free;
  end;
end;

procedure TfmPrincipal.btSerialClick(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbxPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil( frConfiguraSerial );
  end;
end;

procedure TfmPrincipal.CarregarLote1Click(Sender: TObject);
var
  i: Integer;
begin
  OpenDialog1.Filter := 'Arquivo XML|*.xml';
  if OpenDialog1.Execute then
  begin
    ACBrSAT1.LerLoteCFe( OpenDialog1.FileName );
  end;

  mLog.Lines.Clear;

  if ACBrSAT1.LoteCFe.Count > 0 then
  begin
    for i := 0 to ACBrSAT1.LoteCFe.Count -1 do
      mLog.Lines.Add(ACBrSAT1.LoteCFe[i].infCFe.ID + ' - ' +
                     IntToStr(ACBrSAT1.LoteCFe[i].ide.nCFe) + ' - ' +
                     DateToStr(ACBrSAT1.LoteCFe[i].ide.dEmi));

    mLog.Lines.Add('Qtde: ' + IntToStr(ACBrSAT1.LoteCFe.Count));
  end;

  if ACBrSAT1.LoteCFeCanc.Count > 0 then
  begin
    for i := 0 to ACBrSAT1.LoteCFeCanc.Count -1 do
      mLog.Lines.Add(ACBrSAT1.LoteCFeCanc.Items[i].infCFe.ID + ' - ' +
                     IntToStr(ACBrSAT1.LoteCFeCanc.Items[i].ide.nCFe) + ' - ' +
                     DateToStr(ACBrSAT1.LoteCFeCanc.Items[i].ide.dEmi) +
                     ' - Cancelada');

    mLog.Lines.Add('Qtde: ' + IntToStr(ACBrSAT1.LoteCFeCanc.Count));
  end;
end;

procedure TfmPrincipal.cbxFormatXMLChange(Sender: TObject);
begin
  ACBrSAT1.CFe.IdentarXML := cbxFormatXML.Checked;
  ACBrSAT1.CFeCanc.IdentarXML := cbxFormatXML.Checked;
end;

procedure TfmPrincipal.cbxModeloChange(Sender : TObject);
begin
  try
    ACBrSAT1.Modelo := TACBrSATModelo( cbxModelo.ItemIndex );
  except
    cbxModelo.ItemIndex := Integer( ACBrSAT1.Modelo );
    raise;
  end;
end;

procedure TfmPrincipal.cbxMotorRelatorioChange(Sender: TObject);
var
  LSelectedItemIndex: Integer;
  LSelectedObject: TObject;
begin
  LSelectedObject := nil;
  LSelectedItemIndex := cbxMotorRelatorio.ItemIndex;

  if LSelectedItemIndex <> -1 then
    LSelectedObject := TObject(cbxMotorRelatorio.Items.Objects[LSelectedItemIndex]);

  if Assigned(LSelectedObject) then
  begin
    ACBrSAT1.Extrato := TACBrSATExtratoReportClass(LSelectedObject);

    {$IFDEF GERADOR_FAST_REPORT}
      if ACBrSAT1.Extrato is TACBrSATExtratoFR then
        TACBrSATExtratoFR(ACBrSAT1.Extrato).FastExtrato := edtPathFR3.Text;
    {$ENDIF}
  end;

  gbESCPOS.Enabled := cbxMotorRelatorio.Text = 'ESCPOS';

end;

procedure TfmPrincipal.cbxRedeProxyChange(Sender: TObject);
begin
  edRedeProxyIP.Enabled := (cbxRedeProxy.ItemIndex > 0);
  edRedeProxyPorta.Enabled := edRedeProxyIP.Enabled;
  edRedeProxyUser.Enabled  := edRedeProxyIP.Enabled;
  edRedeProxySenha.Enabled := edRedeProxyIP.Enabled;
end;

procedure TfmPrincipal.cbxRemoverAcentosChange(Sender: TObject);
begin
  ACBrSAT1.CFe.RetirarAcentos := cbxRemoverAcentos.Checked;
  ACBrSAT1.CFeCanc.RetirarAcentos := cbxRemoverAcentos.Checked;
end;

procedure TfmPrincipal.cbxSalvarCFeCancChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SalvarCFeCanc := cbxSalvarCFeCanc.Checked;
end;

procedure TfmPrincipal.cbxSalvarCFeChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SalvarCFe := cbxSalvarCFe.Checked;
end;

procedure TfmPrincipal.cbxSalvarEnvioChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SalvarEnvio := cbxSalvarEnvio.Checked;
end;

procedure TfmPrincipal.cbxSepararPorAnoChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorAno := cbxSepararPorAno.Checked;
end;

procedure TfmPrincipal.cbxSepararPorCNPJChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
end;

procedure TfmPrincipal.cbxSepararPorDiaChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorDia := cbxSepararPorDia.Checked;
end;

procedure TfmPrincipal.cbxSepararPorMesChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorMes := cbxSepararPorMes.Checked;
end;

procedure TfmPrincipal.cbxSepararPorModeloChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorModelo := cbxSepararPorModelo.Checked;
end;

procedure TfmPrincipal.cbxUTF8Change(Sender: TObject);
begin
  ACBrSAT1.Config.EhUTF8 := cbxUTF8.Checked;
  sePagCod.Value := ACBrSAT1.Config.PaginaDeCodigo;
end;

procedure TfmPrincipal.MenuItem10Click(Sender: TObject);
begin
  ConfiguraRedeSAT;
  LoadXML( ACBrSAT1.Rede.AsXMLString,  mRede );

  ACBrSAT1.ConfigurarInterfaceDeRede( );
end;

procedure TfmPrincipal.MenuItem13Click(Sender: TObject);
begin
  ConfiguraRedeSAT;
  LoadXML( ACBrSAT1.Rede.AsXMLString,  mRede );

  PageControl1.ActivePage := tsRedeXML;

  SaveDialog1.Filter   := 'Arquivo XML|*.xml';
  SaveDialog1.FileName := 'Rede.xml';
  if SaveDialog1.Execute then
  begin
    ACBrSAT1.Rede.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfmPrincipal.MenuItem15Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo XML|*.xml';
  if OpenDialog1.Execute then
  begin
    ACBrSAT1.CFe.LoadFromFile( OpenDialog1.FileName );
    if (pos('<Signature', ACBrSAT1.CFe.AsXMLString) > 0) then
    begin
      LoadXML( ACBrSAT1.CFe.AsXMLString, mRecebido );
      PageControl1.ActivePage := tsRecebido;
    end
    else
    begin
      mVendaEnviar.Lines.Text := ACBrSAT1.CFe.AsXMLString;
      PageControl1.ActivePage := tsGerado;
    end;
  end;
end;

procedure TfmPrincipal.MenuItem17Click(Sender: TObject);
begin
  ACBrSAT1.CFe.Clear;
  ACBrSAT1.CFeCanc.Clear;

  ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha := rSim;

  mVendaEnviar.Lines.LoadFromFile('C:\Pascal\Comp\ACBr\trunk2\Exemplos\ACBrSAT\Lazarus\Vendas\11111111111111\201509\AD35150911111111111111591234567890001757849146.xml');
  ACBrSAT1.CFe.AsXMLString := mVendaEnviar.Lines.Text;

  mVendaEnviar.Lines.LoadFromFile('C:\Pascal\Comp\ACBr\trunk2\Exemplos\ACBrSAT\Lazarus\Cancelamentos\11111111111111\201509\ADC35150911111111111111591234567890001768086718.xml');
  ACBrSAT1.CFeCanc.AsXMLString := mVendaEnviar.Lines.Text;

  PrepararImpressao;
  ACBrSAT1.ImprimirExtratoCancelamento;
end;

procedure TfmPrincipal.MenuItem18Click(Sender: TObject);
var
  tini, tfim: TDateTime;
begin
  PrepararImpressao;
  tini := now;
  mLog.Lines.Add(ACBrSATExtratoESCPOS1.GerarImpressaoFiscalMFe);
  tfim := now;
  mLog.Lines.Add('Inciado em: '+DateTimeToStr(tini));
  mLog.Lines.Add('Finalizado em: '+DateTimeToStr(tFim));
  mLog.Lines.Add('Diferença: '+ FormatFloat('###.##',SecondSpan(tini,tfim))+' segundos' );
end;

procedure TfmPrincipal.MenuItem19Click(Sender: TObject);
var
  Erro: String;
begin
  ACBrSAT1.Config.XmlSignLib := TSSLXmlSignLib(cbxXmlSignLib.ItemIndex);
  ACBrSAT1.Config.ArqSchema := edSchemaVendaAPL.Text;
  PageControl1.ActivePageIndex := 0;

  if ACBrSAT1.ValidarDadosVenda( mVendaEnviar.Text, Erro ) then
    mLog.Lines.Add('XML Gerado pela aplicação, validado com sucesso')
  else
  begin
    mLog.Lines.Add('Erro na Validação do XML Gerado pela aplicação.');
    mLog.Lines.Add(Erro);
  end;
end;

procedure TfmPrincipal.MenuItem22Click(Sender: TObject);
var
  Erro: String;
begin
  ACBrSAT1.Config.XmlSignLib := TSSLXmlSignLib(cbxXmlSignLib.ItemIndex);
  ACBrSAT1.Config.ArqSchema := edSchemaVendaSAT.Text;
  PageControl1.ActivePageIndex := 0;

  if ACBrSAT1.ValidarDadosVenda( ACBrSAT1.CFe.AsXMLString, Erro ) then
    mLog.Lines.Add('XML Recebido do SAT, validado com sucesso')
  else
  begin
    mLog.Lines.Add('Erro na Validação do XML Recebido do SAT.');
    mLog.Lines.Add(Erro);
  end;
end;

procedure TfmPrincipal.miGerarXMLCancelamentoClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo XML|*.xml';
  if OpenDialog1.Execute then
  begin
    ACBrSAT1.CFe.LoadFromFile( OpenDialog1.FileName );
    ACBrSAT1.CFe2CFeCanc;

    mCancelamentoEnviar.Lines.Text := ACBrSAT1.CFeCanc.GerarXML( True );  // True = Gera apenas as TAGs da aplicação
    edChaveCancelamento.Text := ACBrSAT1.CFeCanc.infCFe.chCanc;
    PageControl1.ActivePage := tsCancelamento;
  end;
end;

procedure TfmPrincipal.miEnviarCancelamentoClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLog;
  if mCancelamentoEnviar.Lines.Count < 1 then
  begin
    ACBrSAT1.CancelarUltimaVenda;
    mCancelamentoEnviar.Lines.Text := ACBrSAT1.CFeCanc.GerarXML(True);
  end
  else
  begin
    if edChaveCancelamento.Text = '' then
    begin
      ACBrSAT1.CFeCanc.AsXMLString := mCancelamentoEnviar.Lines.Text;
      edChaveCancelamento.Text := ACBrSAT1.CFeCanc.infCFe.chCanc;
    end;

    ACBrSAT1.CancelarUltimaVenda( edChaveCancelamento.Text, mCancelamentoEnviar.Lines.Text );
  end;

  if ACBrSAT1.Resposta.codigoDeRetorno = 7000 then
  begin
    LoadXML( ACBrSAT1.CFeCanc.AsXMLString, mRecebido );
    PageControl1.ActivePage := tsRecebido;
  end;
end;

procedure TfmPrincipal.miImprimirExtratoCancelamentoClick(Sender: TObject);
begin
  //ACBrSAT1.CFeCanc.LoadFromFile('C:\temp\ADC35160666591991000132590000371860052583129800.xml');
  //ACBrSAT1.CFe.LoadFromFile('C:\temp\AD35160666591991000132590000371860052570122411.xml');
  PrepararImpressao;
  ACBrSAT1.ImprimirExtratoCancelamento;
end;

procedure TfmPrincipal.mTesteFimAFimClick(Sender: TObject);
//var
//  Numero: Integer;
begin
  if mVendaEnviar.Text = '' then
    mGerarVenda.Click;

//  Numero := ACBrSAT1.CFe.ide.nCFe;
  PageControl1.ActivePage := tsLog;

  ACBrSAT1.TesteFimAFim( mVendaEnviar.Text );

  if ACBrSAT1.Resposta.codigoDeRetorno = 9000 then
  begin
    LoadXML( ACBrSAT1.CFe.AsXMLString, mRecebido );
    PageControl1.ActivePage := tsRecebido;
  end;
end;

procedure TfmPrincipal.rgRedeTipoInterClick(Sender: TObject);
begin
  gbWiFi.Visible := (rgRedeTipoInter.ItemIndex = 1);
end;

procedure TfmPrincipal.rgRedeTipoLanClick(Sender: TObject);
begin
  gbPPPoE.Visible := (rgRedeTipoLan.ItemIndex = 1);
  gbIPFix.Visible := (rgRedeTipoLan.ItemIndex = 2);
end;

procedure TfmPrincipal.sbFR3Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo FR3|*.fr3';
  OpenDialog1.InitialDir := ExtractFilePath(edtPathFR3.Text);
  OpenDialog1.FileName := edtPathFR3.Text;
  if OpenDialog1.Execute then
  begin
    edtPathFR3.Text := OpenDialog1.FileName;
    {$IFDEF GERADOR_FAST_REPORT}
      FACBrSATExtratoFR1.FastExtrato := edtPathFR3.Text;
    {$ENDIF}

  end;
end;

procedure TfmPrincipal.sbNomeDLLClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo DLL|*.dll';
  OpenDialog1.InitialDir := ExtractFilePath(edNomeDLL.Text);
  OpenDialog1.FileName := edNomeDLL.Text;
  if OpenDialog1.Execute then
    edNomeDLL.Text := OpenDialog1.FileName;
end;

procedure TfmPrincipal.sbSchemaVendaAPLClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo XSD|*.xsd';
  OpenDialog1.InitialDir := ExtractFilePath(edSchemaVendaAPL.Text);
  OpenDialog1.FileName := edSchemaVendaAPL.Text;
  if OpenDialog1.Execute then
    edSchemaVendaAPL.Text := OpenDialog1.FileName;
end;

procedure TfmPrincipal.sbSchemaVendaSATClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo XSD|*.xsd';
  OpenDialog1.InitialDir := ExtractFilePath(edSchemaVendaSAT.Text);
  OpenDialog1.FileName := edSchemaVendaSAT.Text;
  if OpenDialog1.Execute then
    edSchemaVendaSAT.Text := OpenDialog1.FileName;
end;

procedure TfmPrincipal.mAtivarSATClick(Sender : TObject);
begin
  ACBrSAT1.AtivarSAT( 1, edtEmitCNPJ.Text, StrToInt(edtCodUF.Text) );
end;

procedure TfmPrincipal.mBloquearSATClick(Sender : TObject);
begin
  ACBrSAT1.BloquearSAT;
end;

procedure TfmPrincipal.mComunicarCertificadoClick(Sender : TObject);
var
  SL : TStringList;
begin
  OpenDialog1.Filter := 'Certificado|*.cer|Arquivo Texto|*.txt';
  if OpenDialog1.Execute then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile( OpenDialog1.FileName );

      ACBrSAT1.ComunicarCertificadoICPBRASIL( SL.Text );
    finally
      SL.Free;
    end;
  end;
end;

procedure TfmPrincipal.mConfigurarInterfaceRedeClick(Sender : TObject);
begin
  OpenDialog1.Filter := 'Arquivo XML|*.xml';
  if OpenDialog1.Execute then
  begin
    ACBrSAT1.Rede.LoadFromFile( OpenDialog1.FileName );

    LeDadosRedeSAT;
    ACBrSAT1.ConfigurarInterfaceDeRede( );
  end;
end;

procedure TfmPrincipal.mConsultarNumeroSessaoClick(Sender : TObject);
Var
  strSessao: String;
  nSessao : Integer;
begin
  strSessao := '';
  if not InputQuery('Consultar Número de Sessão',
                    'Entre com o Número de Sessão a ser consultada:', strSessao ) then
    Exit;

  nSessao := StrToIntDef(strSessao, 0);
  if nSessao <= 0 then
    raise Exception.Create('Numero de sessão informado é inválido');

  ACBrSAT1.ConsultarNumeroSessao( nSessao );
end;

procedure TfmPrincipal.ConsultarltimaSessoFiscal1Click(Sender: TObject);
begin
  ACBrSAT1.ConsultarUltimaSessaoFiscal;
end;

procedure TfmPrincipal.mConsultarSATClick(Sender : TObject);
begin
  ACBrSAT1.ConsultarSAT;
end;

procedure TfmPrincipal.mConsultarStatusOperacionalClick(Sender : TObject);
begin
  ACBrSAT1.ConsultarStatusOperacional;

  if ACBrSAT1.Resposta.codigoDeRetorno = 10000 then
  begin
    with ACBrSAT1.Status do
    begin
      mLog.Lines.Add('NSERIE.........: '+NSERIE);
      mLog.Lines.Add('LAN_MAC........: '+LAN_MAC);
      mLog.Lines.Add('STATUS_LAN.....: '+StatusLanToStr(STATUS_LAN));
      mLog.Lines.Add('NIVEL_BATERIA..: '+NivelBateriaToStr(NIVEL_BATERIA));
      mLog.Lines.Add('MT_TOTAL.......: '+MT_TOTAL);
      mLog.Lines.Add('MT_USADA.......: '+MT_USADA);
      mLog.Lines.Add('DH_ATUAL.......: '+DateTimeToStr(DH_ATUAL));
      mLog.Lines.Add('VER_SB.........: '+VER_SB);
      mLog.Lines.Add('VER_LAYOUT.....: '+VER_LAYOUT);
      mLog.Lines.Add('ULTIMO_CFe.....: '+ULTIMO_CFe);
      mLog.Lines.Add('LISTA_INICIAL..: '+LISTA_INICIAL);
      mLog.Lines.Add('LISTA_FINAL....: '+LISTA_FINAL);
      mLog.Lines.Add('DH_CFe.........: '+DateTimeToStr(DH_CFe));
      mLog.Lines.Add('DH_ULTIMA......: '+DateTimeToStr(DH_ULTIMA));
      mLog.Lines.Add('CERT_EMISSAO...: '+DateToStr(CERT_EMISSAO));
      mLog.Lines.Add('CERT_VENCIMENTO: '+DateToStr(CERT_VENCIMENTO));
      mLog.Lines.Add('ESTADO_OPERACAO: '+EstadoOperacaoToStr(ESTADO_OPERACAO));
    end;

    LeDadosRedeSAT;
  end
  else
    mLog.Lines.Add(ACBrSAT1.Resposta.RetornoStr);
end;

procedure TfmPrincipal.mDesbloquearSATClick(Sender : TObject);
begin
  ACBrSAT1.DesbloquearSAT;
end;

procedure TfmPrincipal.MenuItem5Click(Sender : TObject);
Var
  CodNovo, CodAtual, tipoCodigo: String;
begin
  CodNovo    := '';
  CodAtual   := edtCodigoAtivacao.Text;
  tipoCodigo := '1';

  if not InputQuery('Trocar Código de Ativação',
                    'Entre com o Código de Ativação ou de Emergência:', CodAtual ) then
    Exit;

  if not InputQuery('Trocar Código de Ativação',
                    'Qual o Tipo do Código Informado anteriormente ?'+sLineBreak+
                    '1 – Código de Ativação'+sLineBreak+
                    '2 – Código de Ativação de Emergência'+sLineBreak,
                    tipoCodigo ) then
    Exit;

  if not InputQuery('Trocar Código de Ativação',
                    'Entre com o Número do Novo Código de Ativação:', CodNovo ) then
    Exit;

  ACBrSAT1.TrocarCodigoDeAtivacao( CodAtual, StrToInt(tipoCodigo), CodNovo );

  if ACBrSAT1.Resposta.codigoDeRetorno = 1800 then
  begin
    edtCodigoAtivacao.Text := CodNovo;
    mLog.Lines.Add('Código de Ativação trocado com sucesso');
    btSalvarParams.Click;
  end;
end;

procedure TfmPrincipal.mEnviarVendaClick(Sender : TObject);
var
  tini, tfim: TDateTime;
begin
  if mVendaEnviar.Text = '' then
    mGerarVenda.Click;

  PageControl1.ActivePage := tsLog;

  tini := now;
  ACBrSAT1.EnviarDadosVenda( mVendaEnviar.Text );
  tfim := now;
  mLog.Lines.Add('------------------------------------------------');
  mLog.Lines.Add('Iniciado em: '+DateTimeToStr(tini));
  mLog.Lines.Add('Finalizado em: '+DateTimeToStr(tFim));
  mLog.Lines.Add('');
  mLog.Lines.Add('Tempo de Envio e Recebimento: '+ FormatFloat('##0.00',SecondSpan(tini,tfim))+' segundos' );
  mLog.Lines.Add('------------------------------------------------');

  if ACBrSAT1.Resposta.codigoDeRetorno = 6000 then
  begin
    LoadXML( ACBrSAT1.CFe.AsXMLString, mRecebido );
    PageControl1.ActivePage := tsRecebido;

{    ACBrSAT1.CFe.ide.dEmi;
    ACBrSAT1.CFe.infCFe.ID;
    ACBrSAT1.CFe.Total.vCFe;
    ACBrSAT1.CFe.Dest.CNPJCPF;
    ACBrSAT1.CFe.ide.assinaturaQRCODE;      }

  end;
end;

procedure TfmPrincipal.mExtrairLogsClick(Sender : TObject);
Var
  NomeArquivo: String;
begin
  NomeArquivo := ExtractFilePath(Application.ExeName)+'SAT.LOG';
  if not InputQuery('ExtrairLogs',
                    'Informe o nome para criação do Arquivo de Log:', NomeArquivo ) then
    Exit;

  ACBrSAT1.ExtrairLogs( NomeArquivo );
end;

procedure TfmPrincipal.mGerarVendaClick(Sender : TObject);
var
  TotalItem, TotalGeral, Pagto1: Double;
  A: Integer;
  Loops: Integer;
begin
  TotalGeral := 0;
  PageControl1.ActivePage := tsGerado;

  ACBrSAT1.CFe.IdentarXML := cbxFormatXML.Checked;
  ACBrSAT1.CFe.TamanhoIdentacao := 3;
  ACBrSAT1.CFe.RetirarAcentos := cbxRemoverAcentos.Checked;

  mVendaEnviar.Clear;

  // Trasnferindo Informações de Config para o CFe //
  AjustaACBrSAT;
  ACBrSAT1.InicializaCFe;

  // Montando uma Venda //
  with ACBrSAT1.CFe do
  begin
    ide.numeroCaixa := 1;
    ide.cNF := Random(999999);

    Dest.CNPJCPF := '18760540000139';
    Dest.xNome := 'Projeto ACBr ÁÉÍÓÚáéíóúÇç teste de nome Longo';

    Entrega.xLgr := 'logradouro';
    Entrega.nro := '112233';
    Entrega.xCpl := 'complemento';
    Entrega.xBairro := 'bairro';
    Entrega.xMun := 'municipio';
    Entrega.UF := 'RJ';

    Loops := max(Trunc(seItensVenda.Value / 3)-1, 0);

    for A := 0 to Loops do  // Ajuste aqui para vender mais itens
    begin
      with Det.New do
      begin
        nItem := 1 + (A * 3);
        Prod.cProd := 'ACBR0001';
        Prod.cEAN := '6291041500213';
        Prod.xProd := 'Assinatura SAC';
        prod.NCM := '99';
        Prod.CFOP := '5120';
        Prod.uCom := 'UN';
        Prod.qCom := 1;
        Prod.vUnCom := 120.00;
        Prod.indRegra := irArredondamento;
        Prod.vDesc := 1;
        Prod.CEST := '1234567';

//        with Prod.obsFiscoDet.Add do
//        begin
//          xCampoDet := 'campo';
//          xTextoDet := '999999999';
//        end;

        TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro - Prod.vDesc, -2);
        TotalGeral := TotalGeral + TotalItem;
        Imposto.vItem12741 := TotalItem * 0.12;

        Imposto.ICMS.orig := oeNacional;
        if Emit.cRegTrib = RTSimplesNacional then
          Imposto.ICMS.CSOSN := csosn102
        else
          Imposto.ICMS.CST := cst00;

        Imposto.ICMS.pICMS := 18;

        Imposto.PIS.CST := pis49;
        Imposto.PIS.vBC := TotalItem;
        Imposto.PIS.pPIS := 0.0065;

        Imposto.COFINS.CST := cof49;
        Imposto.COFINS.vBC := TotalItem;
        Imposto.COFINS.pCOFINS := 0.0065;
        //
        //Imposto.COFINSST.vBC := 87206.46;
        //Imposto.COFINSST.pCOFINS := 1.8457;

        infAdProd := 'Informacoes adicionais';
      end;

      with Det.New do
      begin
        nItem := 2 + (A * 3);
        Prod.cProd := '6291041500213';
        Prod.cEAN := '6291041500213';
        Prod.xProd := 'Outro produto Qualquer, com a Descrição Grande';
        Prod.CFOP := '5529';
        Prod.uCom := 'un';
        Prod.qCom := 1.1205;
        Prod.vUnCom := 1.210;
        Prod.indRegra := irArredondamento;
        Prod.vOutro := 2;

//        with Prod.obsFiscoDet.Add do
//        begin
//          xCampoDet := 'campo';
//          xTextoDet := '999999999';
//        end;

        TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro - Prod.vDesc, -2);
        TotalGeral := TotalGeral + TotalItem;
        Imposto.vItem12741 := TotalItem * 0.30;

        Imposto.ICMS.orig := oeNacional;
        if Emit.cRegTrib = RTSimplesNacional then
          Imposto.ICMS.CSOSN := csosn400
        else
          Imposto.ICMS.CST := cst40;

        Imposto.PIS.CST := pis49;
        Imposto.PIS.qBCProd := TotalItem;
        Imposto.PIS.vAliqProd := 1.0223;

        Imposto.PISST.qBCProd := TotalItem;
        Imposto.PISST.vAliqProd := 1.0223;

        Imposto.COFINS.CST := cof49;
        Imposto.COFINS.qBCProd := TotalItem;
        Imposto.COFINS.vAliqProd := 1.0223;

        //Imposto.COFINSST.qBCProd := 503.6348;
        //Imposto.COFINSST.vAliqProd := 779.4577;
        infAdProd := 'Informacoes adicionais';
      end;

      with Det.New do
      begin
        nItem := 3 + (A * 3);
        Prod.cProd := 'abc123';
        Prod.cEAN := '6291041500213';
        Prod.xProd := 'ACBrSAT rules';
        Prod.NCM := '99';
        Prod.CFOP := '5844';
        Prod.uCom := 'un';
        Prod.qCom := 1.1205;
        Prod.vUnCom := 1.210;
        Prod.indRegra := irArredondamento;

//        with Prod.obsFiscoDet.Add do
//        begin
//          xCampoDet := 'campo';
//          xTextoDet := 'abc123';
//        end;

        TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro - Prod.vDesc, -2);
        TotalGeral := TotalGeral + TotalItem;

        Imposto.ICMS.orig := oeEstrangeiraImportacaoDireta;
        if Emit.cRegTrib = RTSimplesNacional then
          Imposto.ICMS.CSOSN := csosn102
        else
          Imposto.ICMS.CST := cst60;

        Imposto.PIS.CST := pis49;

        Imposto.PISST.qBCProd := TotalItem;
        Imposto.PISST.vAliqProd := 1.1826;

        Imposto.COFINS.CST := cof49;

        infAdProd := 'Informacoes adicionais';
      end;
    end;
    (*
    with Det.Add do
    begin
      nItem := 4;
      Prod.cProd := 'abc123';
      Prod.cEAN := '6291041500213';
      Prod.xProd := 'Nada';
      Prod.CFOP := '5025';
      Prod.uCom := 'horas';
      Prod.qCom := 1.1205;
      Prod.vUnCom := 1.210;
      Prod.vProd := 8;
      Prod.indRegra := irTruncamento;
      Prod.vOutro := 93.31;

      Imposto.ICMS.orig := oeEstrangeiraAdquiridaBrasil;
      Imposto.ICMS.CSOSN := csosn900;
      Imposto.ICMS.pICMS := 1.1234;

      Imposto.PIS.CST := pis49;

      Imposto.PISST.qBCProd := 7528.8947;
      Imposto.PISST.vAliqProd := 296.2348;

      Imposto.COFINS.CST := cof49;
    end;
    *)

    Total.DescAcrEntr.vDescSubtot := 5;
    Total.vCFeLei12741 := 1.23;

    Pagto1 := RoundABNT(TotalGeral/2,-2);

    with Pagto.New do
    begin
      cMP := mpCartaodeCredito;
      vMP := Pagto1;
    end;

    with Pagto.New do
    begin
      cMP := mpDinheiro;
      vMP := TotalGeral - Pagto1 + 100;
    end;

    InfAdic.infCpl := 'Acesse www.projetoacbr.com.br para obter mais;informações sobre o componente ACBrSAT;'+
                      'Precisa de um PAF-ECF homologado?;Conheça o Projeto ACBr - www.pojetoacbr.com.br';

   { InfAdic.infCpl := '</linha_simples>;'+
                        '</ce><e><n>SENHA XXX</n></e>;'+
                        '</linha_simples>';}
  end;

  mVendaEnviar.Lines.Text := ACBrSAT1.CFe.GerarXML( True );    // True = Gera apenas as TAGs da aplicação

  mLog.Lines.Add('Venda Gerada');
end;

procedure TfmPrincipal.mImprimirExtratoVendaClick(Sender : TObject);
var
  tini, tfim: TDateTime;
begin
  PrepararImpressao;
  //ACBrSAT1.CFe.LoadFromFile('C:\temp\AD35180111111111111111591234567890001128242021.xml');
  //ACBrSAT1.CFe.LoadFromFile('C:\temp\AD35180111111111111111591234567890001185486520.xml');
  tini := now;
  //ACBrSATExtratoFortes1.Filtro := fiPDF;
  //ACBrSATExtratoFortes1.ImprimirExtrato;
  ACBrSAT1.ImprimirExtrato;
  tfim := now;
  mLog.Lines.Add('Inciado em: '+DateTimeToStr(tini));
  mLog.Lines.Add('Finalizado em: '+DateTimeToStr(tFim));
  mLog.Lines.Add('Diferença: '+ FormatFloat('###.##',SecondSpan(tini,tfim))+' segundos' );
end;

procedure TfmPrincipal.mImprimirExtratoVendaResumidoClick(Sender : TObject);
begin
  PrepararImpressao;
  ACBrSAT1.ImprimirExtratoResumido;
end;

procedure TfmPrincipal.mLimparClick(Sender : TObject);
begin
  mVendaEnviar.Clear;
  LoadXML( '', mRecebido );
  mCancelamentoEnviar.Clear;
end;

procedure TfmPrincipal.SbArqLogClick(Sender : TObject);
begin
  OpenURL( ExtractFilePath( Application.ExeName ) + edLog.Text);
end;

procedure TfmPrincipal.sePagCodChange(Sender: TObject);
begin
  ACBrSAT1.Config.PaginaDeCodigo := sePagCod.Value;
//  cbxUTF8.Checked := ACBrSAT1.Config.EhUTF8;
end;

procedure TfmPrincipal.ConfiguraRedeSAT;
begin
  with ACBrSAT1.Rede do
  begin
    tipoInter   := TTipoInterface( rgRedeTipoInter.ItemIndex );
    SSID        := edRedeSSID.Text;
    seg         := TSegSemFio( cbxRedeSeg.ItemIndex );
    codigo      := edRedeCodigo.Text;
    tipoLan     := TTipoLan( rgRedeTipoLan.ItemIndex );
    lanIP       := edRedeIP.Text;
    lanMask     := edRedeMask.Text;
    lanGW       := edRedeGW.Text;
    lanDNS1     := edRedeDNS1.Text;
    lanDNS2     := edRedeDNS2.Text;
    usuario     := edRedeUsuario.Text;
    senha       := edRedeSenha.Text;
    proxy       := cbxRedeProxy.ItemIndex;
    proxy_ip    := edRedeProxyIP.Text;
    proxy_porta := edRedeProxyPorta.Value;
    proxy_user  := edRedeProxyUser.Text;
    proxy_senha := edRedeProxySenha.Text;
  end;
end;

procedure TfmPrincipal.LeDadosRedeSAT;
begin
  with ACBrSAT1.Rede do
  begin
    rgRedeTipoInter.ItemIndex := Integer(tipoInter);
    edRedeSSID.Text           := SSID;
    cbxRedeSeg.ItemIndex      := Integer(seg);
    edRedeCodigo.Text         := codigo;
    rgRedeTipoLan.ItemIndex   := Integer(tipoLan);
    edRedeIP.Text             := lanIP;
    edRedeMask.Text           := lanMask;
    edRedeGW.Text             := lanGW;
    edRedeDNS1.Text           := lanDNS1;
    edRedeDNS2.Text           := lanDNS2;
    edRedeUsuario.Text        := usuario;
    edRedeSenha.Text          := senha;
    cbxRedeProxy.ItemIndex    := proxy;
    edRedeProxyIP.Text        := proxy_ip;
    edRedeProxyPorta.Value    := proxy_porta;
    edRedeProxyUser.Text      := proxy_user;
    edRedeProxySenha.Text     := proxy_senha;
  end;
end;

procedure TfmPrincipal.PrepararImpressao;
begin
  if ACBrSAT1.Extrato = ACBrSATExtratoESCPOS1 then
  begin
    ACBrPosPrinter1.Desativar;
    ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModeloPosPrinter.ItemIndex );
    ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
    ACBrPosPrinter1.Porta := cbxPorta.Text;
    ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
    ACBrPosPrinter1.LinhasEntreCupons := seLinhasPular.Value;
    ACBrPosPrinter1.EspacoEntreLinhas := seEspLinhas.Value;
    ACBrSATExtratoESCPOS1.ImprimeQRCode := True;
    if cbImprimirChaveUmaLinha.Checked then
      ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha := rSim
    else
      ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha := rAuto;
  end
  else
  begin
    {$IFDEF GERADOR_FORTES_REPORT}
    if ACBrSAT1.Extrato = FACBrSATExtratoFortes1 then
    begin
      FACBrSATExtratoFortes1.LarguraBobina := seLargura.Value;
      FACBrSATExtratoFortes1.MargemSuperior := seMargemTopo.Value;
      FACBrSATExtratoFortes1.MargemInferior := seMargemFundo.Value;
      FACBrSATExtratoFortes1.MargemEsquerda := seMargemEsquerda.Value;
      FACBrSATExtratoFortes1.MargemDireita := seMargemDireita.Value;
      FACBrSATExtratoFortes1.MostraPreview := cbPreview.Checked;

      if lImpressora.Caption <> '' then
        FACBrSATExtratoFortes1.Impressora := lImpressora.Caption;
    end;
    {$ENDIF}

    {$IFDEF GERADOR_FAST_REPORT}
    if ACBrSAT1.Extrato = FACBrSATExtratoFR1 then
    begin
      FACBrSATExtratoFR1.LarguraBobina := seLargura.Value;
      FACBrSATExtratoFR1.MargemSuperior := seMargemTopo.Value;
      FACBrSATExtratoFR1.MargemInferior := seMargemFundo.Value;
      FACBrSATExtratoFR1.MargemEsquerda := seMargemEsquerda.Value;
      FACBrSATExtratoFR1.MargemDireita := seMargemDireita.Value;
      FACBrSATExtratoFR1.MostraPreview := cbPreview.Checked;

      try
        if lImpressora.Caption <> '' then
          FACBrSATExtratoFR1.Impressora := lImpressora.Caption;
      except
      end;
    end;
    {$ENDIF}

  end;

  ACBrSAT1.Extrato.ImprimeLogoLateral    := cbLogoLateral.Checked;
  ACBrSAT1.Extrato.ImprimeQRCodeLateral  := cbQRCodeLateral.Checked;
  ACBrSAT1.Extrato.ImprimeEmUmaLinha     := cbImprimir1Linha.Checked;
  ACBrSAT1.Extrato.ImprimeDescAcrescItem := cbImprimirDescAcres.Checked;
end;

procedure TfmPrincipal.GetsignAC(var Chave: AnsiString);
begin
  Chave := AnsiString( edtSwHAssinatura.Text );
end;

procedure TfmPrincipal.GetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := AnsiString( edtCodigoAtivacao.Text );
end;

procedure TfmPrincipal.sfeVersaoEntChange(Sender: TObject);
begin
  ACBrSAT1.Config.infCFe_versaoDadosEnt := StringToFloatDef(sfeVersaoEnt.Text, cversaoDadosEnt);
end;

procedure TfmPrincipal.LoadXML(const AXML: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT( PathWithDelim(ExtractFileDir(application.ExeName))+MyWebBrowser.Name+'-temp.xml',
              AXML, False, False);
  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName))+MyWebBrowser.Name+'-temp.xml');
end;



end.


