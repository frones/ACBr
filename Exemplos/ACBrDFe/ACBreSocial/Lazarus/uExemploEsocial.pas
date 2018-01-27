{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit uExemploEsocial;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, Buttons, DateUtils, Spin, ExtCtrls, ComCtrls,
  ACBrDFe, ACBrUtil, ACBreSocial, ACBrMail,
  pcnConversao;

//  OleCtrls, SHDocVw,

type
  TFExemploEsocial = class(TForm)
    ACBreSocial1: TACBreSocial;
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
    GroupBox5: TGroupBox;
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
    GroupBox6: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    Label31: TLabel;
    Label32: TLabel;
    Label42: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbxAtualizarXML: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
    cbxRetirarAcentos: TCheckBox;
    cbVersaoDF: TComboBox;
    edtPathSchemas: TEdit;
    TabSheet7: TTabSheet;
    GroupBox7: TGroupBox;
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
    sbPatheSocial: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPatheSocial: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPatheSocial: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
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
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    cbS1000: TCheckBox;
    cbS1010: TCheckBox;
    cbS1020: TCheckBox;
    cbS1030: TCheckBox;
    cbS1040: TCheckBox;
    cbS1080: TCheckBox;
    cbS2100: TCheckBox;
    cbS1070: TCheckBox;
    cbS1060: TCheckBox;
    cbS1050: TCheckBox;
    cbS1005: TCheckBox;
    cbS1035: TCheckBox;
    GroupBox3: TGroupBox;
    cbS1220: TCheckBox;
    cbS1298: TCheckBox;
    cbS1299: TCheckBox;
    cbS1300: TCheckBox;
    cbS1280: TCheckBox;
    cbS1270: TCheckBox;
    cbS1250: TCheckBox;
    cbS1260: TCheckBox;
    cbS1200: TCheckBox;
    cbS1210: TCheckBox;
    cbS1202: TCheckBox;
    cbS1207: TCheckBox;
    cbS1295: TCheckBox;
    GroupBox2: TGroupBox;
    cbS2200: TCheckBox;
    cbS2220: TCheckBox;
    cbS2240: TCheckBox;
    cbS2210: TCheckBox;
    cbS2250: TCheckBox;
    cbS2298: TCheckBox;
    cbAviso: TComboBox;
    cbS3000: TCheckBox;
    cbS4000: TCheckBox;
    cbS4999: TCheckBox;
    cbS2190: TCheckBox;
    cbS2205: TCheckBox;
    cbS2206: TCheckBox;
    cbS2230: TCheckBox;
    cbS2241: TCheckBox;
    cbS2299: TCheckBox;
    cbS2300: TCheckBox;
    cbS2306: TCheckBox;
    cbS2399: TCheckBox;
    cbS2400: TCheckBox;
    GroupBox4: TGroupBox;
    btnGerar: TButton;
    OpenDialog1: TOpenDialog;
    ACBrMail1: TACBrMail;



    procedure btnGerarClick(Sender: TObject);

    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnListaCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure btnValidadeDataClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubjectNameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
    procedure btnSHA_RSAClick(Sender: TObject);
    procedure btnHTTPSClick(Sender: TObject);
    procedure btnX509Click(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure sbPatheSocialClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure ACBreSocial1StatusChange(Sender: TObject);
    procedure ACBreSocial1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);

    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PathClick(Sender: TObject);
  private
    { Private declarations }
    //procedures eventos de tabela
    procedure GerareSocial1000;
    procedure GerareSocial1005;
    procedure GerareSocial1010;
    procedure GerareSocial1020;
    procedure GerareSocial1030;
    procedure GerareSocial1035;
    procedure GerareSocial1040;
    procedure GerareSocial1050;
    procedure GerareSocial1060;
    procedure GerareSocial1070;
    procedure GerareSocial1080;
    procedure GerareSocial2100;

    //procedure eventos periodicos
    procedure GerareSocial1200;
    procedure GerareSocial1202;
    procedure GerareSocial1207;
    procedure GerareSocial1210;
//    procedure GerareSocial1220;
    procedure GerareSocial1250;
    procedure GerareSocial1260;
    procedure GerareSocial1270;
    procedure GerareSocial1280;
    procedure GerareSocial1295;
    procedure GerareSocial1298;
    procedure GerareSocial1299;
    procedure GerareSocial1300;

    //procedures eventos não periódicos
    procedure GerareSocial2190;
    procedure GerareSocial2200;
    procedure GerareSocial2205;
    procedure GerareSocial2206;
    procedure GerareSocial2210;
    procedure GerareSocial2220;
    procedure GerareSocial2230;
    procedure GerareSocial2240;
    procedure GerareSocial2241;
    procedure GerareSocial2250;
    procedure GerareSocial2298;
    procedure GerareSocial2299;
    procedure GerareSocial2300;
    procedure GerareSocial2306;
    procedure GerareSocial2399;
    procedure GerareSocial2400;
    procedure GerareSocial3000;
    procedure GerareSocial4000;
//    procedure GerareSocial4999;

    procedure AtualizaSSLLibsCombo;
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
  public
    { Public declarations }
  end;

var
  FExemploEsocial: TFExemploEsocial;

implementation

uses
  TypInfo, blcksock, Math,
  ACBrDFeConfiguracoes, ACBrDFeSSL,
  Unit2,
  pcesConversaoeSocial;

//  Grids,

const
  SELDIRHELP = 1000;

{$R *.lfm}

{ TFExemploEsocial }

procedure TFExemploEsocial.GerareSocial1000;
var
  i: Integer;
begin
  ACBreSocial1.Eventos.Iniciais.S1000.Clear;

  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Iniciais.S1000.Add do
    begin
      evtInfoEmpregador.id := '1';

      evtInfoEmpregador.IdeEvento.TpAmb := taProducaoRestritaDadosFicticios;
      evtInfoEmpregador.IdeEvento.ProcEmi := TpProcEmi(0);
      evtInfoEmpregador.IdeEvento.VerProc := '1.0';

      evtInfoEmpregador.IdeEmpregador.TpInsc := tiCPF;
      evtInfoEmpregador.IdeEmpregador.NrInsc := '0123456789';

      evtInfoEmpregador.ModoLancamento := TModoLancamento(i);
      evtInfoEmpregador.InfoEmpregador.IdePeriodo.IniValid := '2015-05';
      evtInfoEmpregador.InfoEmpregador.IdePeriodo.FimValid := '2099-12';

      with evtInfoEmpregador.InfoEmpregador.InfoCadastro do
      begin
        NmRazao := 'Empresa Teste';
        ClassTrib := ct01;
        NatJurid := '0001';
        IndCoop := TpIndCoop(1);
        IndConstr := TpIndConstr(2);
        IndDesFolha := TpIndDesFolha(1);
        IndOptRegEletron := TpIndOptRegEletron(1);
        IndEtt := tpSimNao(1);
        nrRegEtt := '';

        InfoOp.nrSiafi := '12345';

        InfoOp.infoEnte.nmEnte := 'Ente federativo teste';
        InfoOp.infoEnte.uf := tpuf(ufSP);
        InfoOp.infoEnte.vrSubteto := 100.00;

        dadosIsencao.IdeMinLei := 'Sigla Min';
        dadosIsencao.NrCertif := '1111';
        dadosIsencao.DtEmisCertif := date;
        dadosIsencao.DtVencCertif := date;
        dadosIsencao.NrProtRenov := '10';
        dadosIsencao.DtProtRenov := date;
        dadosIsencao.DtDou := date;
        dadosIsencao.PagDou := '111';

        Contato.NmCtt := 'Contato 1';
        Contato.CpfCtt := '00000222220';
        Contato.FoneFixo := '34335856';
        Contato.FoneCel := '991524587';
        Contato.email := 'testecontato@testecontato.com';

        InfoOrgInternacional.IndAcordoIsenMulta := tpIndAcordoIsencaoMulta(1);
      end;

      evtInfoEmpregador.InfoEmpregador.InfoCadastro.SoftwareHouse.Clear;

      with evtInfoEmpregador.InfoEmpregador.InfoCadastro.SoftwareHouse.Add do
      begin
        CnpjSoftHouse := '00000000000000';
        NmRazao := 'SoftwareHouse Teste';
        NmCont := 'Soft Contato';
        Telefone := '34335856';
        email := 'teste@teste.com';
      end;

      with evtInfoEmpregador.InfoEmpregador.InfoCadastro.InfoComplementares do
      begin
        SituacaoPJ.IndSitPJ := tpIndSitPJ(0);
        SituacaoPF.IndSitPF := tpIndSitPF(0);
      end;

      evtInfoEmpregador.infoEmpregador.NovaValidade.IniValid := '2014-05';
      evtInfoEmpregador.infoEmpregador.novaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1005;
var
i : Integer;
begin
  ACBreSocial1.Eventos.Iniciais.S1005.Clear;

  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Iniciais.S1005.Add do
    begin
      evtTabEstab.id     := '1';
//      evtTabEstab.Versao := '2.0';

      evtTabEstab.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabEstab.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabEstab.IdeEvento.VerProc := '1.0';

      evtTabEstab.IdeEmpregador.TpInsc := tiCPF;
      evtTabEstab.IdeEmpregador.NrInsc := '0123456789';

      evtTabEstab.ModoLancamento  := TModoLancamento( i );
      evtTabEstab.infoEstab.IdeEstab.tpInsc   := tiCNPJ;
      evtTabEstab.infoEstab.IdeEstab.nrInsc   := '012345678901234';
      evtTabEstab.infoEstab.IdeEstab.iniValid := '2015-05';
      evtTabEstab.infoEstab.IdeEstab.fimValid := '2099-12';

      evtTabEstab.infoEstab.DadosEstab.cnaePrep := '2015';

      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.AliqRat      := arat1;
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.Fap          := 1.5;
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.AliqRatAjust := 2.5;

      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.tpProc := tpTpProc(1);
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.nrProc := '20150512';
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.codSusp := '1';

      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.tpProc := tpTpProc(1);
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.nrProc := '20150512';
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.codSusp := '2';

      evtTabEstab.infoEstab.DadosEstab.infoCaepf.tpCaepf := tcContrIndividual;

      evtTabEstab.infoEstab.DadosEstab.infoObra.indSubstPatrObra := tpIndSubstPatronalObra(1);

      evtTabEstab.infoEstab.DadosEstab.infoTrab.regPt := tpRegPt(3);

      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.contApr := tpContApr(2);
      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud := '20150612';
      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.contEntEd := tpSim;
      with evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.infoEntEduc.Add do
           nrInsc := '0123456789';

      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoPCD.contPCD := tpContPCD(9);
      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoPCD.nrProcJud := '20160131';

      evtTabEstab.infoEstab.NovaValidade.IniValid := '2014-05';
      evtTabEstab.infoEstab.novaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1010;
var
i : Integer;
begin
  ACBreSocial1.Eventos.Tabelas.S1010.Clear;

  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1010.Add do
    begin
      evtTabRubrica.id     := '1';
//      evtTabRubrica.Versao := '2.0';

      evtTabRubrica.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabRubrica.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabRubrica.IdeEvento.VerProc := '1.0';

      evtTabRubrica.IdeEmpregador.TpInsc := tiCPF;
      evtTabRubrica.IdeEmpregador.NrInsc := '0123456789';

      evtTabRubrica.ModoLancamento  := TModoLancamento( i );

      evtTabRubrica.infoRubrica.IdeRubrica.CodRubr    := '5445';
      evtTabRubrica.infoRubrica.IdeRubrica.ideTabRubr := '100000';
      evtTabRubrica.infoRubrica.IdeRubrica.iniValid   := '2015-05';
      evtTabRubrica.infoRubrica.IdeRubrica.fimValid   := '2015-06';

      evtTabRubrica.infoRubrica.DadosRubrica.dscRubr    := 'Teste de S-1010';
      evtTabRubrica.infoRubrica.DadosRubrica.natRubr    := 1022;
      evtTabRubrica.infoRubrica.DadosRubrica.tpRubr     := tpTpRubr(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncCP   := tpCodIncCP(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncIRRF := tpCodIncIRRF(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncFGTS := tpCodIncFGTS(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncSIND := tpCodIncSIND(1);
      evtTabRubrica.infoRubrica.DadosRubrica.observacao := 'Rubrica Teste';

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoCP.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoCP.add do
      begin
        nrProc     := '1020';
        ExtDecisao := tpExtDecisao(1);
        codSusp := '1';
      end;

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoIRRF.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoIRRF.add do
      begin
        nrProc := '1020';
        codSusp := '2';
      end;

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoFGTS.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoFGTS.add do
      begin
        nrProc := '50740';
      end;

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoSIND.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoSIND.add do
      begin
        nrProc := '50';
      end;

      if (EvtTabRubrica.ModoLancamento = mlAlteracao) then
      begin
        evtTabRubrica.InfoRubrica.novaValidade.IniValid := '2015-05';
        evtTabRubrica.InfoRubrica.novaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1020;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1020.Add do
    begin
      evtTabLotacao.id     := '1';
//      evtTabLotacao.Versao := '2.0';

      evtTabLotacao.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabLotacao.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabLotacao.IdeEvento.VerProc := '1.0';

      evtTabLotacao.IdeEmpregador.TpInsc := tiCPF;
      evtTabLotacao.IdeEmpregador.NrInsc := '0123456789';

      evtTabLotacao.ModoLancamento  := TModoLancamento( i );

      evtTabLotacao.infoLotacao.IdeLotacao.codLotacao := '300000';
      evtTabLotacao.infoLotacao.IdeLotacao.iniValid   := '2015-06';
      evtTabLotacao.infoLotacao.IdeLotacao.fimValid   := '2099-12';

      evtTabLotacao.infoLotacao.DadosLotacao.tpLotacao := '01';
      evtTabLotacao.infoLotacao.DadosLotacao.tpInsc    := tiCAEPF;
      evtTabLotacao.infoLotacao.DadosLotacao.nrInsc    := '6564646565';

      evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.Fpas     := '515';
      evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.codTercs := '0015';
      evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.codTercsSusp := '0506';

      with evtTabLotacao.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Add do
      begin
        codTerc   := '1111';
        nrProcJud := '1234567891239-1345';
        codSusp := '1';
      end;

      with evtTabLotacao.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Add do
      begin
        codTerc   := '2222';
        nrProcJud := '1234567891239-1345';
        codSusp := '2';
      end;

      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.tpInscContrat := tpTpInscContratante(0);
      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.NrInscContrat := '74563214500045';
      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.tpInscProp    := TpTpInscProp(0);
      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.nrInscProp    := '654234523416';

      evtTabLotacao.infoLotacao.novaValidade.IniValid := '2015-06';
      evtTabLotacao.infoLotacao.novaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1030;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1030.Add do
    begin
      evtTabCargo.id     := '1';
 //     evtTabCargo.Versao := '2.0';

      evtTabCargo.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabCargo.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabCargo.IdeEvento.VerProc := '1.0';

      evtTabCargo.IdeEmpregador.TpInsc := tiCPF;
      evtTabCargo.IdeEmpregador.NrInsc := '0123456789';

      evtTabCargo.ModoLancamento := TModoLancamento( i );

      evtTabCargo.infoCargo.IdeCargo.CodCargo := '37';
      evtTabCargo.infoCargo.ideCargo.iniValid := '2015-05';
      evtTabCargo.infoCargo.ideCargo.fimValid := '2099-12';

      evtTabCargo.infoCargo.DadosCargo.nmCargo := 'Programador';
      evtTabCargo.infoCargo.DadosCargo.codCBO  := '500000';

      evtTabCargo.infoCargo.DadosCargo.cargoPublico.acumCargo   := tpAcumCargo(0);
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.contagemEsp := tpContagemEsp(0);
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.dedicExcl   := tpSimNao(0);

      evtTabCargo.infoCargo.DadosCargo.cargoPublico.leiCargo.nrLei    := '11111';
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.leiCargo.dtLei    := Now;
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.leiCargo.sitCargo := tpSitCargo(0);

      evtTabCargo.infoCargo.NovaValidade.IniValid := '2015-05';
      evtTabCargo.infoCargo.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1035;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1035.Add do
    begin
      evtTabCarreira.id := '1';
 //     evtTabCargo.Versao := '2.0';

      evtTabCarreira.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabCarreira.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabCarreira.IdeEvento.VerProc := '1.0';

      evtTabCarreira.IdeEmpregador.TpInsc := tiCPF;
      evtTabCarreira.IdeEmpregador.NrInsc := '0123456789';

      evtTabCarreira.ModoLancamento := TModoLancamento( i );

      evtTabCarreira.InfoCarreira.ideCarreira.codCarreira := '1';
      evtTabCarreira.InfoCarreira.ideCarreira.iniValid := '2015-05';
      evtTabCarreira.InfoCarreira.ideCarreira.iniValid := '2099-12';

      evtTabCarreira.InfoCarreira.dadosCarreira.dscCarreira := 'Juiz';
      evtTabCarreira.InfoCarreira.dadosCarreira.leiCarr := 'lei89489/77';
      evtTabCarreira.InfoCarreira.dadosCarreira.dtLeiCarr := now;
      evtTabCarreira.InfoCarreira.dadosCarreira.sitCarr := tpSitCarr(0);

      evtTabCarreira.InfoCarreira.NovaValidade.IniValid := '2015-05';
      evtTabCarreira.InfoCarreira.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1040;
var
  i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1040.Add do
    begin
      evtTabFuncao.id     := '1';
//      evtTabFuncao.Versao := '2.0';

      evtTabFuncao.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabFuncao.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabFuncao.IdeEvento.VerProc := '1.0';

      evtTabFuncao.IdeEmpregador.TpInsc := tiCPF;
      evtTabFuncao.IdeEmpregador.NrInsc := '0123456789';

      evtTabFuncao.ModoLancamento := TModoLancamento( i );

      evtTabFuncao.InfoFuncao.IdeFuncao.CodFuncao := '1';
      evtTabFuncao.InfoFuncao.IdeFuncao.iniValid  := '2015-05';
      evtTabFuncao.InfoFuncao.IdeFuncao.fimValid  := '2099-12';

      evtTabFuncao.InfoFuncao.DadosFuncao.dscFuncao := 'PROGRAMADOR';
      evtTabFuncao.InfoFuncao.DadosFuncao.codCBO    := '5000';

      evtTabFuncao.InfoFuncao.NovaValidade.IniValid := '2015-05';
      evtTabFuncao.InfoFuncao.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1050;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1050.Add do
    begin
      evtTabHorContratual.id     := '1';
//      evtTabHorContratual.Versao := '2.0';

      evtTabHorContratual.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabHorContratual.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabHorContratual.IdeEvento.VerProc := '1.0';

      evtTabHorContratual.IdeEmpregador.TpInsc := tiCPF;
      evtTabHorContratual.IdeEmpregador.NrInsc := '0123456789';

      evtTabHorContratual.ModoLancamento := TModoLancamento( i );

      evtTabHorContratual.InfoHorContratual.ideHorContratual.codHorContrat := '1';
      evtTabHorContratual.InfoHorContratual.ideHorContratual.iniValid      := '2015-05';
      evtTabHorContratual.InfoHorContratual.ideHorContratual.fimValid      := '2099-12';

      evtTabHorContratual.InfoHorContratual.dadosHorContratual.hrEntr         := '0800';
      evtTabHorContratual.InfoHorContratual.dadosHorContratual.hrSaida        := '1800';
      evtTabHorContratual.InfoHorContratual.dadosHorContratual.durJornada     := 525;
      evtTabHorContratual.InfoHorContratual.dadosHorContratual.perHorFlexivel := tpSimNao(1);


      with evtTabHorContratual.InfoHorContratual.dadosHorContratual.horarioIntervalo.Add do
      begin
        tpInterv   := tptpIntervalo(0);
        durInterv  := 90;
        iniInterv  := '1200';
        termInterv := '1330';
      end;

      with evtTabHorContratual.InfoHorContratual.dadosHorContratual.horarioIntervalo.Add do
      begin
        tpInterv   := tptpIntervalo(1);
        durInterv  := 15;
        iniInterv  := '1645';
        termInterv := '1700';
      end;

      evtTabHorContratual.InfoHorContratual.novaValidade.IniValid := '2015-05';
      evtTabHorContratual.InfoHorContratual.novaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1060;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1060.Add do
    begin
      EvtTabAmbiente.id     := '1';
//      EvtTabAmbiente.Versao := '2.0';

      EvtTabAmbiente.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      EvtTabAmbiente.IdeEvento.ProcEmi := TpProcEmi(0);
      EvtTabAmbiente.IdeEvento.VerProc := '1.0';

      EvtTabAmbiente.IdeEmpregador.TpInsc := tiCPF;
      EvtTabAmbiente.IdeEmpregador.NrInsc := '0123456789';

      EvtTabAmbiente.ModoLancamento := TModoLancamento( i );

      EvtTabAmbiente.infoAmbiente.ideAmbiente.codAmb   := '123456';
      EvtTabAmbiente.infoAmbiente.ideAmbiente.iniValid := '2015-05';
      EvtTabAmbiente.infoAmbiente.ideAmbiente.fimValid := '2099-12';

      EvtTabAmbiente.infoAmbiente.dadosAmbiente.dscAmb   := 'DESCRICAO';
      EvtTabAmbiente.infoAmbiente.dadosAmbiente.localAmb := tpLocalAmb(0);
      EvtTabAmbiente.infoAmbiente.dadosAmbiente.tpInsc   := tpTpInscAmbTab(0);
      EvtTabAmbiente.infoAmbiente.dadosAmbiente.nrInsc   := '123456789';

      with EvtTabAmbiente.infoAmbiente.dadosAmbiente.fatorRisco.add do
        codFatRis := '1111';


      with EvtTabAmbiente.infoAmbiente.dadosAmbiente.fatorRisco.add do
        codFatRis := '2222';

      with EvtTabAmbiente.infoAmbiente.dadosAmbiente.fatorRisco.add do
        codFatRis := '3333';

      EvtTabAmbiente.infoAmbiente.novaValidade.IniValid := '2015-06';
      EvtTabAmbiente.infoAmbiente.novaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1070;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1070.Add do
    begin
      evtTabProcesso.id     := '1';
//      evtTabProcesso.Versao := '2.0';

      evtTabProcesso.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabProcesso.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabProcesso.IdeEvento.VerProc := '1.0';

      evtTabProcesso.IdeEmpregador.TpInsc := tiCPF;
      evtTabProcesso.IdeEmpregador.NrInsc := '0123456789';

      evtTabProcesso.ModoLancamento := TModoLancamento( i );

      evtTabProcesso.InfoProcesso.IdeProcesso.tpProc   := tpTpProc(0);
      evtTabProcesso.InfoProcesso.IdeProcesso.nrProc   := '5000';
      evtTabProcesso.InfoProcesso.IdeProcesso.iniValid := '2014-05';
      evtTabProcesso.InfoProcesso.IdeProcesso.fimValid := '2015-06';


      evtTabProcesso.InfoProcesso.DadosProc.IndAutoria := tpindAutoria(0);
      evtTabProcesso.InfoProcesso.DadosProc.indMatProc := tpIndMatProc(0);

      evtTabProcesso.InfoProcesso.DadosProc.DadosProcJud.UfVara     := 'PR';
      evtTabProcesso.InfoProcesso.DadosProc.DadosProcJud.codMunic   := 5075;
      evtTabProcesso.InfoProcesso.DadosProc.DadosProcJud.IdVara     := '20';

      with evtTabProcesso.InfoProcesso.DadosProc.infoSusp.add do
      begin
        codSusp := '1';
        indSusp := tpIndSusp(0);
        dtDecisao := now;
        indDeposito := tpNao;
      end;

      evtTabProcesso.InfoProcesso.NovaValidade.IniValid := '2015-10';
      evtTabProcesso.InfoProcesso.NovaValidade.FimValid := '2016-10';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1080;
var
i : Integer;
begin
  for I := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1080.Add do
    begin
      evtTabOperPortuario.id     := '1';
//      evtTabOperPortuario.Versao := '2.0';

      evtTabOperPortuario.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
      evtTabOperPortuario.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabOperPortuario.IdeEvento.VerProc := '1.0';

      evtTabOperPortuario.IdeEmpregador.TpInsc := tiCPF;
      evtTabOperPortuario.IdeEmpregador.NrInsc := '0123456789';

      evtTabOperPortuario.ModoLancamento := TModoLancamento( i );

      evtTabOperPortuario.InfoOperPortuario.IdeOperPortuario.cnpjOpPortuario := '29813268000156';
      evtTabOperPortuario.InfoOperPortuario.IdeOperPortuario.iniValid        := '2015-05';
      evtTabOperPortuario.InfoOperPortuario.IdeOperPortuario.fimValid        := '2099-12';

      evtTabOperPortuario.InfoOperPortuario.DadosOperPortuario.aliqRat      := arat1;
      evtTabOperPortuario.InfoOperPortuario.DadosOperPortuario.fap          := 0.5;
      evtTabOperPortuario.InfoOperPortuario.DadosOperPortuario.aliqRatAjust := 5.5;

      evtTabOperPortuario.InfoOperPortuario.NovaValidade.IniValid := '2015-05';
      evtTabOperPortuario.InfoOperPortuario.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2100;
begin
  with ACBreSocial1.Eventos.Iniciais.S2100.Add do
  begin
    EvtCadInicial.id     := '1';
//    EvtCadInicial.Versao := '2.0';

    EvtCadInicial.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtCadInicial.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtCadInicial.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtCadInicial.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtCadInicial.IdeEvento.VerProc  := '1.0';

    EvtCadInicial.IdeEmpregador.TpInsc := tiCPF;
    EvtCadInicial.IdeEmpregador.NrInsc := '0123456789';

    EvtCadInicial.Trabalhador.CpfTrab   := '54564654564';
    EvtCadInicial.Trabalhador.NisTrab   := '65464646464';
    EvtCadInicial.Trabalhador.NmTrab    := 'Empregado teste';
    EvtCadInicial.Trabalhador.Sexo      := 'M';
    EvtCadInicial.Trabalhador.RacaCor   := 1;
    EvtCadInicial.Trabalhador.EstCiv    := 1;
    EvtCadInicial.Trabalhador.GrauInstr := '10';
    EvtCadInicial.Trabalhador.nmSoc     := 'Nome Social';

    EvtCadInicial.Trabalhador.Nascimento.DtNascto   := date;
    EvtCadInicial.Trabalhador.Nascimento.codMunic   := 51268;
    EvtCadInicial.Trabalhador.Nascimento.UF         := 'PR';
    EvtCadInicial.Trabalhador.Nascimento.PaisNascto := '565';
    EvtCadInicial.Trabalhador.Nascimento.PaisNac    := '545';
    EvtCadInicial.Trabalhador.Nascimento.NmMae      := 'teste mae';
    EvtCadInicial.Trabalhador.Nascimento.NmPai      := 'teste pai';

    EvtCadInicial.Trabalhador.Documentos.CTPS.NrCtps    := '56454';
    EvtCadInicial.Trabalhador.Documentos.CTPS.SerieCtps := '646';
    EvtCadInicial.Trabalhador.Documentos.CTPS.UfCtps    := 'PR';

    EvtCadInicial.Trabalhador.Documentos.RIC.NrRic        := '123123';
    EvtCadInicial.Trabalhador.Documentos.RIC.OrgaoEmissor := 'SSP';
    EvtCadInicial.Trabalhador.Documentos.RIC.DtExped      := date;

    EvtCadInicial.Trabalhador.Documentos.RG.NrRg         := '123123';
    EvtCadInicial.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
    EvtCadInicial.Trabalhador.Documentos.RG.DtExped      := date;

    EvtCadInicial.Trabalhador.Documentos.RNE.NrRne        := '123123';
    EvtCadInicial.Trabalhador.Documentos.RNE.OrgaoEmissor := 'SSP';
    EvtCadInicial.Trabalhador.Documentos.RNE.DtExped      := date;

    EvtCadInicial.Trabalhador.Documentos.OC.NrOc         := '999';
    EvtCadInicial.Trabalhador.Documentos.OC.OrgaoEmissor := 'SSP';
    EvtCadInicial.Trabalhador.Documentos.OC.DtExped      := Date;
    EvtCadInicial.Trabalhador.Documentos.OC.DtValid      := Date;

    EvtCadInicial.Trabalhador.Documentos.CNH.nrRegCnh     := '999';
    EvtCadInicial.Trabalhador.Documentos.CNH.DtExped      := Date;
    EvtCadInicial.Trabalhador.Documentos.CNH.ufCnh        := tpuf(ufPR);
    EvtCadInicial.Trabalhador.Documentos.CNH.DtValid      := Date;
    EvtCadInicial.Trabalhador.Documentos.CNH.dtPriHab     := Date;
    EvtCadInicial.Trabalhador.Documentos.CNH.categoriaCnh := tpCnh(cnA);

    EvtCadInicial.Trabalhador.Endereco.Brasil.TpLograd    := 'RUA';
    EvtCadInicial.Trabalhador.Endereco.Brasil.DscLograd   := 'TESTE';
    EvtCadInicial.Trabalhador.Endereco.Brasil.NrLograd    := '777';
    EvtCadInicial.Trabalhador.Endereco.Brasil.Complemento := 'AP 101';
    EvtCadInicial.Trabalhador.Endereco.Brasil.Bairro      := 'CENTRO';
    EvtCadInicial.Trabalhador.Endereco.Brasil.Cep         := '85500000';
    EvtCadInicial.Trabalhador.Endereco.Brasil.CodMunic    := 11111;
    EvtCadInicial.Trabalhador.Endereco.Brasil.UF          := tpuf(ufPR);

    EvtCadInicial.Trabalhador.Endereco.Exterior.PaisResid   := '545';
    EvtCadInicial.Trabalhador.Endereco.Exterior.DscLograd   := 'TESTE';
    EvtCadInicial.Trabalhador.Endereco.Exterior.NrLograd    := '777';
    EvtCadInicial.Trabalhador.Endereco.Exterior.Complemento := 'AP 101';
    EvtCadInicial.Trabalhador.Endereco.Exterior.Bairro      := 'CENTRO';
    EvtCadInicial.Trabalhador.Endereco.Exterior.NmCid       := 'CIDADE EXTERIOR';
    EvtCadInicial.Trabalhador.Endereco.Exterior.CodPostal   := '50000';

    EvtCadInicial.Trabalhador.TrabEstrangeiro.DtChegada        := Date;
    EvtCadInicial.Trabalhador.TrabEstrangeiro.ClassTrabEstrang := tpClassTrabEstrang(ctVistoPermanente);
    EvtCadInicial.Trabalhador.TrabEstrangeiro.CasadoBr         := 'N';
    EvtCadInicial.Trabalhador.TrabEstrangeiro.FilhosBr         := 'N';

    EvtCadInicial.Trabalhador.InfoDeficiencia.DefFisica      := tpNao;
    EvtCadInicial.Trabalhador.InfoDeficiencia.DefVisual      := tpNao;
    EvtCadInicial.Trabalhador.InfoDeficiencia.DefAuditiva    := tpNao;
    EvtCadInicial.Trabalhador.InfoDeficiencia.DefMental      := tpNao;
    EvtCadInicial.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
    EvtCadInicial.Trabalhador.InfoDeficiencia.ReabReadap     := tpSim;
    EvtCadInicial.Trabalhador.InfoDeficiencia.infoCota       := tpNao;
    EvtCadInicial.Trabalhador.InfoDeficiencia.Observacao     := 'sem deficiencia';

    with EvtCadInicial.Trabalhador.Dependente.Add do
    begin
      tpDep    := tdConjuge;
      nmDep    := 'Dependente 1';
      dtNascto := Date;
      cpfDep   := '57548758778';
      depIRRF  := tpSim;
      depSF    := tpNao;
      incTrab  := tpNao;
    end;

    with EvtCadInicial.Trabalhador.Dependente.Add do
    begin
      tpDep    := tdFilhoOuEnteadoAte21Anos; 
      nmDep    := 'Dependente 2';
      dtNascto := Date;
      cpfDep   := '57548758778';
      depIRRF  := tpSim;
      depSF    := tpNao;
      incTrab  := tpNao;
    end;

    EvtCadInicial.Trabalhador.Aposentadoria.TrabAposent := tpNao;

    EvtCadInicial.Trabalhador.Contato.FonePrinc     := '91067240';
    EvtCadInicial.Trabalhador.Contato.FoneAlternat  := '91067240';
    EvtCadInicial.Trabalhador.Contato.EmailPrinc    := 'TESTE@email.com.br';
    EvtCadInicial.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';

    EvtCadInicial.Vinculo.Matricula := '54545';
    EvtCadInicial.Vinculo.TpRegTrab := tpTpRegTrab(1);
    EvtCadInicial.Vinculo.TpRegPrev := tpTpRegPrev(1);

    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.DtAdm             := Date;
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.TpAdmissao        := tpTpAdmissao(1);
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.IndAdmissao       := tpTpIndAdmissao(iaNormal);
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.TpRegJor          := tpTpRegJor(1);
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.NatAtividade      := tpNatAtividade(navUrbano);
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.dtBase            := 03;
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf := '12345678901234';

    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.FGTS.OpcFGTS   := tpOpcFGTS(ofOptante);
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.FGTS.DtOpcFGTS := Date;

    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.hipLeg := 1;
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr := 'Produção de panetones para o natal';
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.tpinclContr := tpInclContr(0);

    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.TpInsc := tiCPF;
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.NrInsc := '564564656';
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.ideEstabVinc.TpInsc := tiCPF;
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.ideEstabVinc.NrInsc := '444564656';

    with EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTrabSubstituido.Add do
      CpfTrabSubst := '12345678901';

    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := tiCPF;
    EvtCadInicial.Vinculo.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := '233564656';

    //Enviar apenas um tipo de regime
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.IndProvim   := tpIndProvim(ipNormal);
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.TpProv      := tpTpProv(tpNomeacaoCargoEfetivo);
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.DtNomeacao  := Date;
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.DtPosse     := Date;
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.DtExercicio := Date;
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.tpPlanRP := tpPlanRP(0);
    //EvtCadInicial.Vinculo.InfoRegimeTrab.InfoEstatutario.infoDecJud.nrProcJud := '313032130';


    EvtCadInicial.Vinculo.InfoContrato.CodCargo  := '545';
    EvtCadInicial.Vinculo.InfoContrato.CodFuncao := '5456';
    EvtCadInicial.Vinculo.InfoContrato.CodCateg  := 111;
    EvtCadInicial.Vinculo.InfoContrato.codCarreira := '1';
    EvtCadInicial.Vinculo.InfoContrato.dtIngrCarr := now;

    EvtCadInicial.Vinculo.InfoContrato.Remuneracao.VrSalFx    := 5000;
    EvtCadInicial.Vinculo.InfoContrato.Remuneracao.UndSalFixo := tpUndSalFixo(5);
    EvtCadInicial.Vinculo.InfoContrato.Remuneracao.DscSalVar  := 'nada a declarar';

    EvtCadInicial.Vinculo.InfoContrato.Duracao.TpContr := tpTpContr(1);
    EvtCadInicial.Vinculo.InfoContrato.Duracao.dtTerm  := Date;

    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.TpInsc   := tiCPF;
    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.NrInsc   := '213864656';
    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.DescComp := 'Descricao logal geral teste';

//    Informar apenas para trabalhador doméstico
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.TpLograd    := '123';
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.DscLograd   := 'LOCAL DOMESTICO';
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.NrLograd    := '111';
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Complemento := 'Complemento';
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Bairro      := 'Bairro';
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Cep         := '85202630';
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.CodMunic    := 123;
//    EvtCadInicial.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Uf          := tpuf(ufPR);

    EvtCadInicial.Vinculo.InfoContrato.HorContratual.QtdHrsSem := 44;
    EvtCadInicial.Vinculo.InfoContrato.HorContratual.TpJornada := tpTpJornada(1);
    EvtCadInicial.Vinculo.InfoContrato.HorContratual.DscTpJorn := 'horario contratual';
    EvtCadInicial.Vinculo.InfoContrato.HorContratual.tmpParc := tpNaoeTempoParcial;

    with EvtCadInicial.Vinculo.InfoContrato.HorContratual.horario.Add do
    begin
      Dia := tpTpDia(diSegundaFeira);
      CodHorContrat := '54';
    end;

    with EvtCadInicial.Vinculo.InfoContrato.HorContratual.horario.Add do
    begin
      Dia := tpTpDia(diTercaFeira);
      CodHorContrat := '10';
    end;

    with EvtCadInicial.Vinculo.InfoContrato.FiliacaoSindical.Add do
      CnpjSindTrab := '45321452000145';

    EvtCadInicial.Vinculo.InfoContrato.AlvaraJudicial.NrProcJud      := '123';

    EvtCadInicial.Vinculo.SucessaoVinc.cnpjEmpregAnt := '12354654000155';
    EvtCadInicial.Vinculo.SucessaoVinc.MatricAnt     := '123';
    EvtCadInicial.Vinculo.SucessaoVinc.dtTransf  := Date;
    EvtCadInicial.Vinculo.SucessaoVinc.Observacao    := 'transferido';

    evtCadInicial.vinculo.Afastamento.DtIniAfast := Date;
    evtCadInicial.vinculo.Afastamento.codMotAfast := '15';
    evtCadInicial.vinculo.Desligamento.DtDeslig  := Date;
  end;
end;

procedure TFExemploEsocial.GerareSocial1200;
begin
  with ACBreSocial1.Eventos.Periodicos.S1200.Add do
  begin
    evtRemun.id     := '1';
//      evtRemun.versao := '2.0';

    evtRemun.ideEvento.indRetif    := ireOriginal;
    //evtRemun.ideEvento.NrRecibo  := '4564654'; Numero do recibo que será retificado.
    evtRemun.ideEvento.IndApuracao := tpIndApuracao(iapuMensal);
    evtRemun.ideEvento.perApur     := '052015';
    evtRemun.ideEvento.TpAmb       := taProducao;
    evtRemun.ideEvento.ProcEmi     := peAplicEmpregador;
    evtRemun.ideEvento.VerProc     := '1.0';

    evtRemun.ideEmpregador.TpInsc  := tiCNPJ;
    evtRemun.ideEmpregador.NrInsc  := '012345678987654';

    evtRemun.ideTrabalhador.cpfTrab    := '01234567890';
    evtRemun.ideTrabalhador.nisTrab    := '09876543210';

    evtRemun.ideTrabalhador.infoMV.indMV := imvDescontadaempregador;

    {Os Grupos abaixo são opcionais
    O grupo abaixocorresponde a funcionários que tenham dois empregos em empresas diferentes }
    with evtRemun.ideTrabalhador.infoMV.remunOutrEmpr.add do
    begin
      tpInsc     := tiCNPJ;
      nrInsc     := '01234567890123';
      codCateg   := 222;
      vlrRemunOE := 1230.10;
    end;

   //o grupo abaixo corresponde apenas a trabalhadores cuja categoria não está sujeita ao evento de admissão
   //   ou TSV-início
    evtRemun.ideTrabalhador.infoComplem.nmTrab       := 'João das Neves';
    evtRemun.ideTrabalhador.infoComplem.dtNascto     := Date;
    evtRemun.ideTrabalhador.infoComplem.codCBO       := '000001';
    evtRemun.ideTrabalhador.infoComplem.natAtividade := navUrbano;
    evtRemun.ideTrabalhador.infoComplem.qtdDiasTrab  := 10;

    evtRemun.ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt := '12345678987654';
    evtRemun.ideTrabalhador.infoComplem.sucessaoVinc.matricAnt := '123';
    evtRemun.ideTrabalhador.infoComplem.sucessaoVinc.dtAdm := now;
    evtRemun.ideTrabalhador.infoComplem.sucessaoVinc.observacao := 'obs sucessao vinc';

    //os dados abaixo só devem ser informados em caso do processo existir e houver decisão que incida sobre as
    //  contribuições
    with evtRemun.ideTrabalhador.procJudTrab.Add do
    begin
      tpTrib := tptPrevidenciaria;
      nrProcJud := '95135703320156150258';
      codSusp := 1;
    end;

    with evtRemun.dmDev.add do
    begin
      ideDmDev := '1';
      codCateg := 111;

      with infoPerApur.ideEstabLot.add do
      begin
        tpInsc     := tiCNPJ;
        nrInsc     := '012345678987654';
        codLotacao := 'SACI54321';
        qtdDiasAv  := 22;

        with remunPerApur.Add do
        begin
          matricula := 'A1234';
          indSimples:= idsIntegralmente;
          with itensRemun.Add do
          begin
            codRubr := '987654';
            ideTabRubr := 'E380';
            qtdRubr := 100;
            fatorRubr := 50;
            vrUnit  := 3296.35;
            vrRubr  := 3330.30;
          end;

          with infoSaudeColet.detOper.Add do
          begin
            cnpjOper := '01234567898765';
            regANS   := 'A1B2C3';
            vrPgTit  := 1.50;
            with detPlano.Add do
            begin
              tpDep    := tdConjuge;
              cpfDep   := '01234567898';
              nmDep    := 'José das Areias';
              dtNascto := Date;
              vlrPgDep := 0.75;
            end;
          end;

          infoAgNocivo.grauExp := ge1;
        end;
      end;

      with infoPerAnt.ideADC.add do
      begin
        dtAcConv := now;
        tpAcConv := tacLegislacaoFederalEstadualMunicipalDistrital;
        dtEfAcConv := now;
        compAcConv := '2017-01';
        dsc := 'Dissídio';
        with idePeriodo.Add do
        begin
          perRef := '201504';
          with ideEstabLot.Add do
          begin
            tpInsc     := tiCNPJ;
            nrInsc     := '01234567898765';
            codLotacao := 'TESTE123';
            with remunPerAnt.Add do
            begin
              matricula  := 'A1234';
              indSimples := idsIntegralmente;
              with itensRemun.Add do
              begin
                codRubr := '987654';
                ideTabRubr := 'E380';
                qtdRubr := 100;
                fatorRubr := 50;
                vrUnit  := 3296.35;
                vrRubr  := 3330.30;
              end;

              infoAgNocivo.grauExp := ge1;

            end;
          end;
        end;
      end;

      with infoTrabInterm.add do
        codConv := '123456';

    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1202;
begin
  with ACBreSocial1.Eventos.Periodicos.S1202.Add do
  begin
    EvtRmnRPPS.id     := '1';
//      evtRemun.versao := '2.0';

    EvtRmnRPPS.ideEvento.indRetif    := ireOriginal;
    //evtRemun.ideEvento.NrRecibo  := '4564654'; Numero do recibo que será retificado.
    EvtRmnRPPS.ideEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtRmnRPPS.ideEvento.perApur     := '052015';
    EvtRmnRPPS.ideEvento.TpAmb       := taProducao;
    EvtRmnRPPS.ideEvento.ProcEmi     := peAplicEmpregador;
    EvtRmnRPPS.ideEvento.VerProc     := '1.0';

    EvtRmnRPPS.ideEmpregador.TpInsc  := tiCNPJ;
    EvtRmnRPPS.ideEmpregador.NrInsc  := '012345678987654';

    EvtRmnRPPS.ideTrabalhador.cpfTrab := '01234567890';
    EvtRmnRPPS.ideTrabalhador.nisTrab := '09876543210';
    EvtRmnRPPS.ideTrabalhador.qtdDepFP := 0;

    //os dados abaixo só devem ser informados em caso do processo existir e houver decisão que incida sobre as  contribuições
    with EvtRmnRPPS.ideTrabalhador.procJudTrab.Add do
    begin
      tpTrib := tptPrevidenciaria;
      nrProcJud := '95135703320156150258';
      codSusp := 1;
    end;

    with EvtRmnRPPS.dmDev.add do
    begin
      ideDmDev := '1';
      with infoPerApur.ideEstab.add do
      begin
        tpInsc := tiCNPJ;
        nrInsc := '012345678987654';
        with remunPerApur.Add do
        begin
          matricula := 'A1234';
          codCateg  := 101;
          with itensRemun.Add do
          begin
            codRubr := '987654';
            ideTabRubr := 'E380';
            qtdRubr := 100;
            fatorRubr := 50;
            vrUnit  := 3296.35;
            vrRubr  := 3330.30;
          end;

          with infoSaudeColet.detOper.Add do
          begin
            cnpjOper := '01234567898765';
            regANS   := 'A1B2C3';
            vrPgTit  := 1.50;
            with detPlano.Add do
            begin
              tpDep    := tdConjuge;
              cpfDep   := '01234567898';
              dtNascto := now;
              nmDep    := 'José das Areias';
              vlrPgDep := 0.75;
            end;
          end;
        end;
      end;

      with infoPerAnt.ideADC.add do
      begin
        dtLei := now;
        nrLei := '321321/2017';
        dtEf  := now;
        with idePeriodo.add do
        begin
          perRef := '2015-03';
          with ideEstab.add do
          begin
            tpInsc := tiCNPJ;
            nrInsc := '01234567898765';
            with remunPerAnt.Add do
            begin
              matricula  := 'A1234';
              codCateg   := 101;
              with itensRemun.Add do
              begin
                codRubr := '987654';
                ideTabRubr := 'E380';
                qtdRubr := 100;
                fatorRubr := 50;
                vrUnit  := 3296.35;
                vrRubr  := 3330.30;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1207;
begin
  with ACBreSocial1.Eventos.Periodicos.S1207.Add do
  begin
    evtBenPrRP.id     := '1';
//    EvtCadInicial.Versao := '2.0';

    evtBenPrRP.IdeEvento.indRetif := tpIndRetificacao(0);
    evtBenPrRP.IdeEvento.NrRecibo := '65.5454.987798798798';
    evtBenPrRP.IdeEvento.IndApuracao:= iapuMensal;
    evtBenPrRP.IdeEvento.perApur := '2017-05';
    evtBenPrRP.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    evtBenPrRP.IdeEvento.ProcEmi  := TpProcEmi(0);
    evtBenPrRP.IdeEvento.VerProc  := '1.0';

    evtBenPrRP.IdeEmpregador.TpInsc := tiCPF;
    evtBenPrRP.IdeEmpregador.NrInsc := '0123456789';

    evtBenPrRP.ideBenef.cpfBenef := '88888888888';

    with evtBenPrRP.dmDev.add do
    begin
      tpBenef := 01;
      nrBenefic := '3132132';
      ideDmDev := '1';
      with itens.add do
      begin
        codRubr := '1';
        ideTabRubr:='E07';
        vrRubr := 110.53;
      end;

      with itens.add do
      begin
        codRubr := '2';
        ideTabRubr:='E08';
        vrRubr := 2568.89;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1210;
//var
//  I : Integer;
begin
  with AcbreSocial1.Eventos.Periodicos.S1210.Add do
  begin
    EvtPgtos.id := '1';
//      EvtPgtos.versao := '2.0';

    EvtPgtos.IdeEvento.indRetif := ireOriginal;
//    EvtPgtos.IdeEvento.NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; - obrigatório se indRetif = ireRetificacao.
    EvtPgtos.IdeEvento.IndApuracao := iapuMensal;
    EvtPgtos.IdeEvento.perApur := '052015';
    EvtPgtos.IdeEvento.TpAmb := taProducao;
    EvtPgtos.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtPgtos.IdeEvento.VerProc := '1.0';

    EvtPgtos.IdeEmpregador.TpInsc := tiCNPJ;
    EvtPgtos.IdeEmpregador.NrInsc := '9632587410123';

    EvtPgtos.IdeBenef.CpfBenef := '01478523690';
    EvtPgtos.IdeBenef.deps.vrDedDep := 100.50;
    with EvtPgtos.IdeBenef.InfoPgto.Add do
    begin
      DtPgto := StrToDate('10/06/2015');
      tpPgto := tpPgtoFl;
      IndResBr := tpNao;
      //-OS GRUPOS ABAIXO SÃO OPCIONAIS
      //grupo detPgtoFl agora é um collection
      with detPgtoFl.Add do
      begin
        perRef := '052015';
        ideDmDev := '2';
        indPagtoTt := tpSim;
        vrLiq := 12365.43;
        nrRecArq := '132156156';
        with retPagtoTot.Add do
        begin
          codRubr := '1';
          ideTabRubr:='0';
          qtdRubr := 1.5;
          fatorRubr := 50;
          vrUnit := 100.10;
          vrRubr := 1001.00;
          with penAlim.add do
          begin
            cpfBenef := '12345698745';
            dtNasctoBenef := now;
            nmBenefic := 'Beneficiário da pensão';
            vlrPensao := 556.32;
          end;
        end;
        with infoPgtoParc.add do
        begin
          codRubr := '2';
          ideTabRubr := '0';
          qtdRubr := 1.5;
          fatorRubr := 0.5;
          vrUnit := 56.85;
          vrRubr := 560.85;
        end;
      end;
      detPgtoBenPr.perRef := '2017-01';
      detPgtoBenPr.ideDmDev := '1';
      detPgtoBenPr.indPgtoTt := tpNao;
      detPgtoBenPr.vrLiq := 1500.21;
      with detPgtoBenPr.retPgtoTot.add do
      begin
        codRubr := '321';
        ideTabRubr := '0';
        qtdRubr := 1.5;
        fatorRubr := 50.65;
        vrUnit := 500.85;
        vrRubr := 5001.65;
      end;
      with detPgtoBenPr.infoPgtoParc.add do
      begin
        codRubr := '555';
        ideTabRubr := '0';
        qtdRubr := 2;
        fatorRubr := 40.11;
        vrUnit := 842.85;
        vrRubr := 774.65;
      end;
      with detPgtoFer.add do
      begin
        codCateg := 111;
        dtIniGoz := now;
        qtDias := 30;
        vrLiq := 2500.32;
        with detRubrFer.add do
        begin
          codRubr := '888';
          ideTabRubr := '0';
          qtdRubr := 1;
          fatorRubr := 100;
          vrUnit := 144.33;
          vrRubr := 2500.32;
          with penAlim.add do
          begin
            cpfBenef := '44455588899';
            dtNasctoBenef := now;
            nmBenefic := 'Beneficiário de Pensão nas Férias';
            vlrPensao := 250.32;
          end;
        end;
      end;
      with detPgtoAnt.add do
      begin
        codCateg := 111;
        with infoPgtoAnt.add do
        begin
          tpBcIRRF := tpCodIncIRRF(0);
          vrBcIRRF := 2500.32;
        end;
      end;

      //grupo idePgtoExt
      idePgtoExt.idePais.codPais := '116';
      IdePgtoExt.idePais.indNIF := infBeneficiaNIF;
      IdePgtoExt.idePais.nifBenef := 'ABCDEFGH123456789';
      IdePgtoExt.endExt.dscLograd := 'Abbey Road St';
      IdePgtoExt.endExt.nrLograd := '93';
      IdePgtoExt.endExt.complem := 'apto 11';
      IdePgtoExt.endExt.bairro := 'Sgt Peppers';
      IdePgtoExt.endExt.nmCid := 'Liverpool';
      IdePgtoExt.endExt.codPostal := '9999999999';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1250;
begin
  with ACBreSocial1.Eventos.Periodicos.S1250.Add do
  begin
    EvtAqProd.id     := '1';
//    EvtAqProd.Versao := '2.0';

    EvtAqProd.IdeEvento.indRetif    := tpIndRetificacao(0);
    EvtAqProd.IdeEvento.NrRecibo    := '65.5454.987798798798';
    EvtAqProd.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtAqProd.IdeEvento.perApur     := '2015-06';
    EvtAqProd.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtAqProd.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtAqProd.IdeEvento.VerProc     := '1.0';

    EvtAqProd.IdeEmpregador.TpInsc := tiCPF;
    EvtAqProd.IdeEmpregador.NrInsc := '0123456789';

    EvtAqProd.InfoAquisProd.IdeEstabAdquir.tpInscAdq := tiCNPJ;
    EvtAqProd.InfoAquisProd.IdeEstabAdquir.nrInscAdq := '12345678910001';

    with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Add do
    begin
      indAquis := tpIdAquis(0);
      vlrTotAquis := 520000.80;

      with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Add do
      begin
        tpInscProd  := tiCNPJ;
        nrInscProd  := '98765432100015';
        vlrBruto    := 4000.54;
        vrCPDescPR  := 3850.32;
        vrRatDescPR := 500.30;
        vrSenarDesc := 2500.30;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[0].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '64896549898789';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[0].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[0].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[0].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[0].InfoProcJud.Add do
          begin
            nrProcJud   := '56464897';
            codSusp     := 333;
            vrCPNRet    := 99999.99;
            vrRatNRet   := 88888.88;
            vrSenarNRet := 77777.77;
          end;
      end;


      with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Add do
      begin
        tpInscProd  := tiCNPJ;
        nrInscProd  := '98765432100015';
        vlrBruto    := 4000.54;
        vrCPDescPR  := 3850.32;
        vrRatDescPR := 500.30;
        vrSenarDesc := 2500.30;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[1].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[1].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[1].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[1].Nfs.Add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0].IdeProdutor.Items[0].InfoProcJud.Add do
          begin
            nrProcJud   := '56464897';
            codSusp     := 222;
            vrCPNRet    := 99999.99;
            vrRatNRet   := 88888.88;
            vrSenarNRet := 77777.77;
          end;
      end;

    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1260;
begin
  with ACBreSocial1.Eventos.Periodicos.S1260.Add do
  begin
    EvtComProd.id     := '1';
//    EvtComProd.Versao := '2.0';

    EvtComProd.IdeEvento.indRetif    := tpIndRetificacao(0);
    EvtComProd.IdeEvento.NrRecibo    := '65.5454.987798798798';
    EvtComProd.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtComProd.IdeEvento.perApur     := '2015-06';
    EvtComProd.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtComProd.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtComProd.IdeEvento.VerProc     := '1.0';

    EvtComProd.IdeEmpregador.TpInsc := tiCPF;
    EvtComProd.IdeEmpregador.NrInsc := '0123456789';

    EvtComProd.InfoComProd.IdeEstabel.nrInscEstabRural := '123456789';

    with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Add do
    begin
      indComerc := tpIndComerc(0);
      vrTotCom  := 5000.80;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
        with nfs.add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
        with nfs.add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;
      end;
    end;

    with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Add do
    begin
      indComerc := tpIndComerc(1);
      vrTotCom  := 5000.80;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        tpInsc   := tiCNPJ;
        nrInsc   := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR  := 9999.99;
        with nfs.add do
        begin
          serie       := '00004';
          nrDocto     := '648965498987894';
          dtEmisNF    := now;
          vlrBruto    := 4000.54;
          vrCPDescPR  := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;
      end;
    end;
    with EvtComProd.InfoComProd.IdeEstabel do
      begin
        nrInscEstabRural := '123654987123';
        with TpComerc.Add do
          begin
            indComerc := icComProdPorProdRuralPFInclusiveSegEspEfetuadaDirVarejoConsFinal;
            vrTotCom := 123456.65;
            with IdeAdquir.Add do
              begin
                tpInsc := tiCNPJ;
                nrInsc := '12345678901';
                vrComerc := 1234569.98;
                vrRetPR := 123654.78;
              end;
            with InfoProcJud.Add do
              begin
                 tpProc := tpAdministrativo;
                 tpTrib := tptIRRF;
                 nrProcJud := '1236548796521';
                 codSusp := 444;
                 vrCPNRet := 123.65;
                 vrRatNRet := 123.65;
                 vrSenarNRet := 123.65;
                 vrCPSusp := 123.65;
                 vrRatSusp := 123.65;
                 vrSenarSusp := 123.63;
              end;
          end;
      end;

  end;
end;

procedure TFExemploEsocial.GerareSocial1270;
begin
  with ACBreSocial1.Eventos.Periodicos.S1270.Add do
  begin
    EvtContratAvNP.id     := '1';
//    EvtContratAvNP.Versao := '2.0';

    EvtContratAvNP.IdeEvento.indRetif    := tpIndRetificacao(0);
    EvtContratAvNP.IdeEvento.NrRecibo    := '65.5454.987798798798';
    EvtContratAvNP.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtContratAvNP.IdeEvento.perApur     := '2015-06';
    EvtContratAvNP.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtContratAvNP.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtContratAvNP.IdeEvento.VerProc     := '1.0';

    EvtContratAvNP.IdeEmpregador.TpInsc := tiCNPJ;
    EvtContratAvNP.IdeEmpregador.NrInsc := '0123456789';

    with EvtContratAvNP.RemunAvNP.Add do
    begin
      tpInsc   := tiCNPJ;
      nrInsc   := '98765432100015';
      codLotacao := '1';
      vrBcCp00 := 650.65;
      vrBcCp15 := 650.65;
      vrBcCp20 := 650.65;
      vrBcCp25 := 650.65;
      vrBcCp13 := 650.65;
      vrBcFgts := 894.65;
      vrDescCP := 500.30;
    end;

    with EvtContratAvNP.RemunAvNP.Add do
    begin
      tpInsc   := tpTpInsc(1);
      nrInsc   := '65432198700015';
      codLotacao := '1';
      vrBcCp00 := 650.65;
      vrBcCp15 := 650.65;
      vrBcCp20 := 650.65;
      vrBcCp25 := 650.65;
      vrBcCp13 := 650.65;
      vrBcFgts := 894.65;
      vrDescCP := 500.30;
    end;


    with EvtContratAvNP.RemunAvNP.Add do
    begin
      tpInsc   := tpTpInsc(2);
      nrInsc   := '98765432100015';
      codLotacao := '1';
      vrBcCp00 := 650.65;
      vrBcCp15 := 650.65;
      vrBcCp20 := 650.65;
      vrBcCp25 := 650.65;
      vrBcCp13 := 650.65;
      vrBcFgts := 894.65;
      vrDescCP := 500.30;
    end;

    with EvtContratAvNP.RemunAvNP.Add do
    begin
      tpInsc   := tpTpInsc(1);
      nrInsc   := '11111111111111';
      codLotacao := '1';
      vrBcCp00 := 650.65;
      vrBcCp15 := 650.65;
      vrBcCp20 := 650.65;
      vrBcCp25 := 650.65;
      vrBcCp13 := 650.65;
      vrBcFgts := 894.65;
      vrDescCP := 500.30;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1280;
begin
  with ACBreSocial1.Eventos.Periodicos.S1280.Add do
  begin
    EvtInfoComplPer.id     := '1';
//    EvtInfoComplPer.Versao := '2.0';

    EvtInfoComplPer.IdeEvento.indRetif    := tpIndRetificacao(0);
    EvtInfoComplPer.IdeEvento.NrRecibo    := '65.5454.987798798798';
    EvtInfoComplPer.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtInfoComplPer.IdeEvento.perApur     := '2015-06';
    EvtInfoComplPer.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtInfoComplPer.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtInfoComplPer.IdeEvento.VerProc     := '1.0';

    EvtInfoComplPer.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtInfoComplPer.IdeEmpregador.NrInsc := '0123456789';

    EvtInfoComplPer.InfoSubstPatr.indSubstPatr   := tpIndSubstPatrOpPort(0);
    EvtInfoComplPer.InfoSubstPatr.percRedContrib := 500.20;

    with EvtInfoComplPer.InfoSubstPatrOpPort.Add do
    begin
      cnpjOpPortuario      := '12345678900112';
    end;

    with EvtInfoComplPer.InfoSubstPatrOpPort.Add do
    begin
      cnpjOpPortuario      := '98765432100014';
    end;

    EvtInfoComplPer.InfoAtivConcom.fatorMes := 9.00;
    EvtInfoComplPer.InfoAtivConcom.fator13  := 1.00;
  end;
end;

procedure TFExemploEsocial.GerareSocial1295;
begin
  with ACBreSocial1.Eventos.Periodicos.S1295.Add do
  begin

    evtTotConting.id     := '1';
//    EvtFechaEvPer.Versao := '2.0';

    evtTotConting.IdeEvento.indRetif    := tpIndRetificacao(0);
    evtTotConting.IdeEvento.NrRecibo    := '65.5454.987798798798';
    evtTotConting.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    evtTotConting.IdeEvento.perApur     := '2015-06';
    evtTotConting.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    evtTotConting.IdeEvento.ProcEmi     := TpProcEmi(0);
    evtTotConting.IdeEvento.VerProc     := '1.0';

    evtTotConting.IdeEmpregador.TpInsc := tpTpInsc(1);
    evtTotConting.IdeEmpregador.NrInsc := '0123456789';

    evtTotConting.IdeRespInf.nmResp   := 'Responsavel teste';
    evtTotConting.IdeRespInf.cpfResp  := '12345678950';
    evtTotConting.IdeRespInf.telefone := '46 - 22222222';
    evtTotConting.IdeRespInf.email    := 'Responsavelteste@email.com';

  end;
end;

procedure TFExemploEsocial.GerareSocial1298;
begin
  with ACBreSocial1.Eventos.Periodicos.S1298.Add do
  begin
    EvtReabreEvPer.id     := '1';
//    EvtReabreEvPer.Versao := '2.0';

    EvtReabreEvPer.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtReabreEvPer.IdeEvento.perApur     := '2015-06';
    EvtReabreEvPer.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtReabreEvPer.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtReabreEvPer.IdeEvento.VerProc     := '1.0';

    EvtReabreEvPer.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtReabreEvPer.IdeEmpregador.NrInsc := '0123456789';
  end;
end;

procedure TFExemploEsocial.GerareSocial1299;
begin
  with ACBreSocial1.Eventos.Periodicos.S1299.Add do
  begin
    EvtFechaEvPer.id     := '1';
//    EvtFechaEvPer.Versao := '2.0';

    EvtFechaEvPer.IdeEvento.indRetif    := tpIndRetificacao(0);
    EvtFechaEvPer.IdeEvento.NrRecibo    := '65.5454.987798798798';
    EvtFechaEvPer.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtFechaEvPer.IdeEvento.perApur     := '2015-06';
    EvtFechaEvPer.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtFechaEvPer.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtFechaEvPer.IdeEvento.VerProc     := '1.0';

    EvtFechaEvPer.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtFechaEvPer.IdeEmpregador.NrInsc := '0123456789';

    EvtFechaEvPer.IdeRespInf.nmResp   := 'Responsavel teste';
    EvtFechaEvPer.IdeRespInf.cpfResp  := '12345678950';
    EvtFechaEvPer.IdeRespInf.telefone := '46 - 22222222';
    EvtFechaEvPer.IdeRespInf.email    := 'Responsavelteste@email.com';

    EvtFechaEvPer.InfoFech.evtRemun        := tpSimNao(0);
    EvtFechaEvPer.InfoFech.evtPgtos        := tpSimNao(1);
    EvtFechaEvPer.InfoFech.evtAqProd       := tpSimNao(1);
    EvtFechaEvPer.InfoFech.evtComProd      := tpSimNao(0);
    EvtFechaEvPer.InfoFech.evtContratAvNP  := tpSimNao(1);
    EvtFechaEvPer.InfoFech.evtInfoComplPer := tpSimNao(0);
    EvtFechaEvPer.InfoFech.compSemMovto    := '07-2015';
  end;
end;

procedure TFExemploEsocial.GerareSocial1300;
begin
  with ACBreSocial1.Eventos.Periodicos.S1300.Add do
  begin
    EvtContrSindPatr.id     := '1';
//    EvtContrSindPatr.Versao := '2.0';

    EvtContrSindPatr.IdeEvento.indRetif    := tpIndRetificacao(0);
    EvtContrSindPatr.IdeEvento.NrRecibo    := '65.5454.987798798798';
    EvtContrSindPatr.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtContrSindPatr.IdeEvento.perApur     := '2015-06';
    EvtContrSindPatr.IdeEvento.TpAmb       := taProducaoRestritaDadosFicticios;
    EvtContrSindPatr.IdeEvento.ProcEmi     := TpProcEmi(0);
    EvtContrSindPatr.IdeEvento.VerProc     := '1.0';

    EvtContrSindPatr.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtContrSindPatr.IdeEmpregador.NrInsc := '0123456789';

    with EvtContrSindPatr.ContribSind.Add do
    begin
      cnpjSindic      := '01234567891111';
      tpContribSind   := tpTpContribSind(0);
      vlrContribSind  := 1500.50;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2190;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2190.Add do
  begin
    EvtAdmPrelim.id     := '1';
//    EvtAdmPrelim.Versao := '2.0';

    EvtAdmPrelim.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtAdmPrelim.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtAdmPrelim.IdeEvento.VerProc  := '1.0';

    EvtAdmPrelim.IdeEmpregador.TpInsc  := tpTpInsc(1);
    EvtAdmPrelim.IdeEmpregador.NrInsc  := '12345678901234';

    EvtAdmPrelim.InfoRegPrelim.cpfTrab := '12345678901';
    EvtAdmPrelim.InfoRegPrelim.dtNascto := now-9125;
    EvtAdmPrelim.InfoRegPrelim.dtAdm := Now;
  end;
end;

procedure TFExemploEsocial.GerareSocial2200;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2200.Add do
  begin
    EvtAdmissao.id     := '1';
//    EvtAdmissao.Versao := '2.0';

 //   EvtAdmissao.IdeEvento.indRetif := tpIndRetificacao(1);
    EvtAdmissao.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAdmissao.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtAdmissao.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtAdmissao.IdeEvento.VerProc  := '1.0';

    EvtAdmissao.IdeEmpregador.TpInsc  := tiCNPJ;
    EvtAdmissao.IdeEmpregador.NrInsc  := '12345678901234';

    EvtAdmissao.Trabalhador.CpfTrab   := '54564654564';
    EvtAdmissao.Trabalhador.NisTrab   := '12345678901';
    EvtAdmissao.Trabalhador.NmTrab    := 'Empregado teste';
    EvtAdmissao.Trabalhador.Sexo      := 'M';
    EvtAdmissao.Trabalhador.RacaCor   := 1;
    EvtAdmissao.Trabalhador.EstCiv    := 1;
    EvtAdmissao.Trabalhador.GrauInstr := '10';
    EvtAdmissao.Trabalhador.IndPriEmpr:= tpNao;
    EvtAdmissao.Trabalhador.nmSoc := 'Nome social';

    EvtAdmissao.Trabalhador.Nascimento.DtNascto   := date;
    EvtAdmissao.Trabalhador.Nascimento.codMunic   := 51268;
    EvtAdmissao.Trabalhador.Nascimento.UF         := 'PR';
    EvtAdmissao.Trabalhador.Nascimento.PaisNascto := '565';
    EvtAdmissao.Trabalhador.Nascimento.PaisNac    := '545';
    EvtAdmissao.Trabalhador.Nascimento.NmMae      := 'teste mae';
    EvtAdmissao.Trabalhador.Nascimento.NmPai      := 'teste pai';

    EvtAdmissao.Trabalhador.Documentos.CTPS.NrCtps    := '56454';
    EvtAdmissao.Trabalhador.Documentos.CTPS.SerieCtps := '646';
    EvtAdmissao.Trabalhador.Documentos.CTPS.UfCtps    := 'PR';

    EvtAdmissao.Trabalhador.Documentos.RIC.NrRic        := '123123';
    EvtAdmissao.Trabalhador.Documentos.RIC.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.RIC.DtExped      := date;

    EvtAdmissao.Trabalhador.Documentos.RG.NrRg         := '123123';
    EvtAdmissao.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.RG.DtExped      := date;

    EvtAdmissao.Trabalhador.Documentos.RNE.NrRne        := '123123';
    EvtAdmissao.Trabalhador.Documentos.RNE.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.RNE.DtExped      := date;

    EvtAdmissao.Trabalhador.Documentos.OC.NrOc         := '999';
    EvtAdmissao.Trabalhador.Documentos.OC.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.OC.DtExped      := Date;
    EvtAdmissao.Trabalhador.Documentos.OC.DtValid      := Date;

    EvtAdmissao.Trabalhador.Documentos.CNH.nrRegCnh     := '999';
    EvtAdmissao.Trabalhador.Documentos.CNH.DtExped      := Date;
    EvtAdmissao.Trabalhador.Documentos.CNH.ufCnh        := tpuf(ufPR);
    EvtAdmissao.Trabalhador.Documentos.CNH.DtValid      := Date;
    EvtAdmissao.Trabalhador.Documentos.CNH.dtPriHab     := Date;
    EvtAdmissao.Trabalhador.Documentos.CNH.categoriaCnh := tpCnh(cnA);

    EvtAdmissao.Trabalhador.Endereco.Brasil.TpLograd    := 'RUA';
    EvtAdmissao.Trabalhador.Endereco.Brasil.DscLograd   := 'TESTE';
    EvtAdmissao.Trabalhador.Endereco.Brasil.NrLograd    := '777';
    EvtAdmissao.Trabalhador.Endereco.Brasil.Complemento := 'AP 101';
    EvtAdmissao.Trabalhador.Endereco.Brasil.Bairro      := 'CENTRO';
    EvtAdmissao.Trabalhador.Endereco.Brasil.Cep         := '85500000';
    EvtAdmissao.Trabalhador.Endereco.Brasil.CodMunic    := 11111;
    EvtAdmissao.Trabalhador.Endereco.Brasil.UF          := tpuf(ufPR);

    EvtAdmissao.Trabalhador.Endereco.Exterior.PaisResid   := '545';
    EvtAdmissao.Trabalhador.Endereco.Exterior.DscLograd   := 'TESTE';
    EvtAdmissao.Trabalhador.Endereco.Exterior.NrLograd    := '777';
    EvtAdmissao.Trabalhador.Endereco.Exterior.Complemento := 'AP 101';
    EvtAdmissao.Trabalhador.Endereco.Exterior.Bairro      := 'CENTRO';
    EvtAdmissao.Trabalhador.Endereco.Exterior.NmCid       := 'CIDADE EXTERIOR';
    EvtAdmissao.Trabalhador.Endereco.Exterior.CodPostal   := '50000';

    EvtAdmissao.Trabalhador.TrabEstrangeiro.DtChegada        := Date;
    EvtAdmissao.Trabalhador.TrabEstrangeiro.ClassTrabEstrang := tpClassTrabEstrang(ctVistoPermanente);
    EvtAdmissao.Trabalhador.TrabEstrangeiro.CasadoBr         := 'N';
    EvtAdmissao.Trabalhador.TrabEstrangeiro.FilhosBr         := 'N';

    EvtAdmissao.Trabalhador.InfoDeficiencia.DefFisica      := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefVisual      := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefAuditiva    := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefMental      := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.ReabReadap     := tpSim;
    EvtAdmissao.Trabalhador.InfoDeficiencia.infoCota       := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.Observacao     := 'sem deficiencia';

    with EvtAdmissao.Trabalhador.Dependente.Add do
    begin
      tpDep    := tdConjuge;
      nmDep    := 'Dependente 1';
      dtNascto := Date;
      cpfDep   := '12345678901';
      depIRRF  := tpSim;
      depSF    := tpNao;
      incTrab  := tpNao;
    end;

    with EvtAdmissao.Trabalhador.Dependente.Add do
    begin
      tpDep    := tdFilhoOuEnteadoAte21Anos;
      nmDep    := 'Dependente 2';
      dtNascto := Date;
      cpfDep   := '12345678901';
      depIRRF  := tpSim;
      depSF    := tpNao;
      incTrab  := tpNao;
    end;

    EvtAdmissao.Trabalhador.Aposentadoria.TrabAposent := tpNao;

    EvtAdmissao.Trabalhador.Contato.FonePrinc     := '91067240';
    EvtAdmissao.Trabalhador.Contato.FoneAlternat  := '91067240';
    EvtAdmissao.Trabalhador.Contato.EmailPrinc    := 'TESTE@email.com.br';
    EvtAdmissao.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';

    EvtAdmissao.Vinculo.Matricula      := '54545';
    EvtAdmissao.Vinculo.TpRegTrab      := tpTpRegTrab(1);
    EvtAdmissao.Vinculo.TpRegPrev      := tpTpRegPrev(1);
    EvtAdmissao.Vinculo.NrRecInfPrelim := '9999999999';

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.DtAdm             := Date;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.TpAdmissao        := tpTpAdmissao(1);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.IndAdmissao       := tpTpIndAdmissao(iaNormal);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.TpRegJor          := tpTpRegJor(1);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.NatAtividade      := tpNatAtividade(navUrbano);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.dtBase            := 03;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf := '12345678901234';

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.FGTS.OpcFGTS   := tpOpcFGTS(ofOptante);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.FGTS.DtOpcFGTS := Date;

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.hipLeg := 1;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr := 'teste';
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.tpinclContr := tpInclContr(0);

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.TpInsc := tiCNPJ;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.NrInsc := '12345678901234';
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.ideEstabVinc.TpInsc := tiCNPJ;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTomadorServ.ideEstabVinc.NrInsc := '12345678901234';

    with EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTrabSubstituido.add do
      CpfTrabSubst := '12345678912';

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := tpTpInsc(1);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := '98765432109';

  // enviar apenas um tipo de admissao
  //  EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.IndProvim   := tpIndProvim(ipNormal);
  //  EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.TpProv      := tpTpProv(tpNomeacaoCargoEfetivo);
  //  EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.DtNomeacao  := Date;
  //  EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.DtPosse     := Date;
  //  EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.DtExercicio := Date;
  //  end;

    EvtAdmissao.Vinculo.InfoContrato.CodCargo  := '545';
    EvtAdmissao.Vinculo.InfoContrato.CodFuncao := '5456';
    EvtAdmissao.Vinculo.InfoContrato.CodCateg  := 111;
    EvtAdmissao.Vinculo.InfoContrato.codCarreira := '1';
    EvtAdmissao.Vinculo.InfoContrato.dtIngrCarr := now;

    EvtAdmissao.Vinculo.InfoContrato.Remuneracao.VrSalFx    := 5000;
    EvtAdmissao.Vinculo.InfoContrato.Remuneracao.UndSalFixo := tpUndSalFixo(5);
    EvtAdmissao.Vinculo.InfoContrato.Remuneracao.DscSalVar  := 'nada a declarar';

    EvtAdmissao.Vinculo.InfoContrato.Duracao.TpContr := tpTpContr(1);
    EvtAdmissao.Vinculo.InfoContrato.Duracao.dtTerm  := Date;

    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.TpInsc   := tiCNPJ;
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.NrInsc   := '21354632';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.DescComp := 'Descricao local geral teste';

    {
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.TpLograd    := '123';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.DscLograd   := 'LOCAL DOMESTICO';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.NrLograd    := '111';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Complemento := 'Complemento';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Bairro      := 'Bairro';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Cep         := '85202630';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.CodMunic    := 123;
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabDom.Uf          := tpuf(ufPR);
    }

    EvtAdmissao.Vinculo.InfoContrato.HorContratual.QtdHrsSem := 44;
    EvtAdmissao.Vinculo.InfoContrato.HorContratual.TpJornada := tpTpJornada(1);
    EvtAdmissao.Vinculo.InfoContrato.HorContratual.DscTpJorn := 'horario contratual';
    EvtAdmissao.Vinculo.InfoContrato.HorContratual.tmpParc := tpNaoeTempoParcial;

    with EvtAdmissao.Vinculo.InfoContrato.HorContratual.horario.Add do
    begin
      Dia := tpTpDia(diSegundaFeira);
      CodHorContrat := '54';
    end;

    with EvtAdmissao.Vinculo.InfoContrato.HorContratual.horario.Add do
    begin
      Dia := tpTpDia(diTercaFeira);
      CodHorContrat := '10';
    end;

    with EvtAdmissao.Vinculo.InfoContrato.FiliacaoSindical.add do
      CnpjSindTrab := '12345678901234';

    EvtAdmissao.Vinculo.InfoContrato.AlvaraJudicial.NrProcJud      := '123';

    with EvtAdmissao.Vinculo.InfoContrato.observacoes.Add do
      observacao := 'Observacao';

    EvtAdmissao.Vinculo.SucessaoVinc.cnpjEmpregAnt                 := '12345678901234';
    EvtAdmissao.Vinculo.SucessaoVinc.MatricAnt                     := '123';
    EvtAdmissao.Vinculo.SucessaoVinc.dtTransf                  := Date;
    EvtAdmissao.Vinculo.SucessaoVinc.Observacao                    := 'transferido';

    EvtAdmissao.Vinculo.transfDom.cpfSubstituido := '12345678901';
    EvtAdmissao.Vinculo.transfDom.matricAnt := '123';
    EvtAdmissao.Vinculo.transfDom.dtTransf := Date;

    EvtAdmissao.Vinculo.afastamento.DtIniAfast := now;
    EvtAdmissao.Vinculo.afastamento.codMotAfast := '01';

    EvtAdmissao.Vinculo.Desligamento.DtDeslig := now;
  end;
end;

procedure TFExemploEsocial.GerareSocial2205;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2205.Add do
  begin
    EvtAltCadastral.id     := '1';
//    EvtAltCadastral.Versao := '2.0';

    EvtAltCadastral.IdeEvento.indRetif := tpIndRetificacao(1);
    EvtAltCadastral.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAltCadastral.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtAltCadastral.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtAltCadastral.IdeEvento.VerProc  := '1.0';

    EvtAltCadastral.IdeEmpregador.TpInsc  := tpTpInsc(1);
    EvtAltCadastral.IdeEmpregador.NrInsc  := '12345678901234';

    EvtAltCadastral.IdeTrabalhador.CpfTrab   := '12345678901';

    EvtAltCadastral.dtAlteracao := NOW;

    EvtAltCadastral.Trabalhador.NisTrab   := '12345678901';
    EvtAltCadastral.Trabalhador.NmTrab    := 'Empregado teste';
    EvtAltCadastral.Trabalhador.Sexo      := 'M';
    EvtAltCadastral.Trabalhador.RacaCor   := 1;
    EvtAltCadastral.Trabalhador.EstCiv    := 1;
    EvtAltCadastral.Trabalhador.GrauInstr := '10';
    EvtAltCadastral.Trabalhador.nmSoc     := 'Nome social';

    EvtAltCadastral.Trabalhador.Documentos.CTPS.NrCtps    := '56454';
    EvtAltCadastral.Trabalhador.Documentos.CTPS.SerieCtps := '646';
    EvtAltCadastral.Trabalhador.Documentos.CTPS.UfCtps    := 'PR';

    EvtAltCadastral.Trabalhador.Documentos.RIC.NrRic        := '123123';
    EvtAltCadastral.Trabalhador.Documentos.RIC.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.RIC.DtExped      := date;

    EvtAltCadastral.Trabalhador.Documentos.RG.NrRg         := '123123';
    EvtAltCadastral.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.RG.DtExped      := date;

    EvtAltCadastral.Trabalhador.Documentos.RNE.NrRne        := '123123';
    EvtAltCadastral.Trabalhador.Documentos.RNE.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.RNE.DtExped      := date;

    EvtAltCadastral.Trabalhador.Documentos.OC.NrOc         := '999';
    EvtAltCadastral.Trabalhador.Documentos.OC.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.OC.DtExped      := Date;
    EvtAltCadastral.Trabalhador.Documentos.OC.DtValid      := Date;

    EvtAltCadastral.Trabalhador.Documentos.CNH.nrRegCnh     := '999';
    EvtAltCadastral.Trabalhador.Documentos.CNH.DtExped      := Date;
    EvtAltCadastral.Trabalhador.Documentos.CNH.ufCnh        := tpuf(ufPR);
    EvtAltCadastral.Trabalhador.Documentos.CNH.DtValid      := Date;
    EvtAltCadastral.Trabalhador.Documentos.CNH.dtPriHab     := Date;
    EvtAltCadastral.Trabalhador.Documentos.CNH.categoriaCnh := tpCnh(cnA);

    EvtAltCadastral.Trabalhador.Endereco.Brasil.TpLograd    := 'RUA';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.DscLograd   := 'TESTE';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.NrLograd    := '777';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.Complemento := 'AP 101';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.Bairro      := 'CENTRO';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.Cep         := '85500000';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.CodMunic    := 11111;
    EvtAltCadastral.Trabalhador.Endereco.Brasil.UF          := tpuf(ufPR);

    EvtAltCadastral.Trabalhador.Endereco.Exterior.PaisResid   := '545';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.DscLograd   := 'TESTE';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.NrLograd    := '777';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.Complemento := 'AP 101';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.Bairro      := 'CENTRO';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.NmCid       := 'CIDADE EXTERIOR';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.CodPostal   := '50000';

    EvtAltCadastral.Trabalhador.TrabEstrangeiro.DtChegada        := Date;
    EvtAltCadastral.Trabalhador.TrabEstrangeiro.ClassTrabEstrang := tpClassTrabEstrang(ctVistoPermanente);
    EvtAltCadastral.Trabalhador.TrabEstrangeiro.CasadoBr         := 'N';
    EvtAltCadastral.Trabalhador.TrabEstrangeiro.FilhosBr         := 'N';

    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefFisica      := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefVisual      := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefAuditiva    := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefMental      := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.ReabReadap     := tpSim;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.infoCota       := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.Observacao     := 'sem deficiencia';

    with EvtAltCadastral.Trabalhador.Dependente.Add do
    begin
      tpDep    := tdConjuge;
      nmDep    := 'Dependente 1';
      dtNascto := Date;
      cpfDep   := '12345678901';
      depIRRF  := tpSim;
      depSF    := tpNao;
      incTrab  := tpNao;
    end;

    with EvtAltCadastral.Trabalhador.Dependente.Add do
    begin
      tpDep    := tdFilhoOuEnteadoAte21Anos;
      nmDep    := 'Dependente 2';
      dtNascto := Date;
      cpfDep   := '12345678901';
      depIRRF  := tpSim;
      depSF    := tpNao;
      incTrab  := tpNao
    end;

    EvtAltCadastral.Trabalhador.Aposentadoria.TrabAposent := tpNao;

    EvtAltCadastral.Trabalhador.Contato.FonePrinc     := '91067240';
    EvtAltCadastral.Trabalhador.Contato.FoneAlternat  := '91067240';
    EvtAltCadastral.Trabalhador.Contato.EmailPrinc    := 'TESTE@email.com.br';
    EvtAltCadastral.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';
  end;
end;

procedure TFExemploEsocial.GerareSocial2206;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2206.Add do
    begin
      EvtAltContratual.id     := '1';
//      EvtAltContratual.versao := '2.0';

      EvtAltContratual.IdeEvento.indRetif := ireOriginal;
//    EvtAltContratual.IdeEvento.NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif = ireRetificacao;
      EvtAltContratual.IdeEvento.TpAmb := taProducaoRestritaDadosFicticios;
      EvtAltContratual.IdeEvento.ProcEmi := peAplicEmpregador;
      EvtAltContratual.IdeEvento.VerProc := '1.0';

      EvtAltContratual.IdeEmpregador.TpInsc := tiCNPJ;
      EvtAltContratual.IdeEmpregador.NrInsc := '12345678901234';

      EvtAltContratual.IdeVinculo.cpfTrab := '12345678901';
      EvtAltContratual.IdeVinculo.nisTrab := '96325874103';
      EvtAltContratual.IdeVinculo.matricula := 'A1234';

      EvtAltContratual.AltContratual.dtALteracao := Date;
      EvtAltContratual.AltContratual.dtEf := now;
      EvtAltContratual.AltContratual.dscAlt := 'descrição da alteração';

      EvtAltContratual.AltContratual.Vinculo.TpRegTrab := trCLT;
      EvtAltContratual.AltContratual.Vinculo.TpRegPrev := rpRGPS;

      EvtAltContratual.AltContratual.infoRegimeTrab.InfoCeletista.TpRegJor := rjSubmetidosHorarioTrabalho;
      EvtAltContratual.AltContratual.infoRegimeTrab.InfoCeletista.NatAtividade := navUrbano;
      EvtAltContratual.AltContratual.infoRegimeTrab.InfoCeletista.dtBase := 08;
      EvtAltContratual.AltContratual.infoRegimeTrab.InfoCeletista.cnpjSindCategProf := '15975395135700';

      EvtAltContratual.AltContratual.infoRegimeTrab.InfoCeletista.TrabTemporario.justProrr := 'Prorrogado porque eu quis';

      //EvtAltContratual.AltContratual.infoRegimeTrab.InfoEstatutario.tpPlanRP := tpPlanRP(0);

      EvtAltContratual.AltContratual.infoContrato.CodCargo := '123';
      EvtAltContratual.AltContratual.infoContrato.CodFuncao := '321';
      EvtAltContratual.AltContratual.infoContrato.CodCateg := 111;
      EvtAltContratual.AltContratual.infoContrato.codCarreira := '1';
      EvtAltContratual.AltContratual.infoContrato.dtIngrCarr := now;

      EvtAltContratual.AltContratual.infoContrato.Remuneracao.VrSalFx := 780.00;
      EvtAltContratual.AltContratual.infoContrato.Remuneracao.UndSalFixo := sfPorMes;
      EvtAltContratual.AltContratual.infoContrato.Remuneracao.DscSalVar := 'Descrição de salário variável, obrigatório caso UndSalFixo for sfNaoAplicavel';

      EvtAltContratual.AltContratual.infoContrato.Duracao.TpContr := PrazoIndeterminado;
//    EvtAltContratual.AltContratual.infoContrato.Duracao.dtTerm  := Date; Obrigatório se TpContr = PrazoDeterminado!

      //LocalTrabGeral não deve ser preenchido no caso de trabalhador doméstico.
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc := tiCNPJ;
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc := '12345678901234';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.DescComp := 'Descrição complementar do local de trabalho.';

      //LocalTrabDom - exclusivo para trabalhador doméstico, indicando endereço onde exerce suas atividades
      (*EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.TpLograd    := '001';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.DscLograd   := 'Rua das Hortencias';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.NrLograd    := '12';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Complemento := 'Fundos';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Bairro      := 'Jardim das Flores';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Cep         := '11001001';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.CodMunic    := 1234567;
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Uf          := ufPr;*)

      EvtAltContratual.AltContratual.infoContrato.HorContratual.QtdHrsSem := 44;
      EvtAltContratual.AltContratual.infoContrato.HorContratual.TpJornada := tjDemaisTiposJornada;
      EvtAltContratual.AltContratual.infoContrato.HorContratual.DscTpJorn := 'Descrição do tipo de jornada, obrigatório se tpJornada = tjDemaisTiposJornada';
      EvtAltContratual.AltContratual.infoContrato.HorContratual.tmpParc := tpNaoeTempoParcial;


      with EvtAltContratual.AltContratual.infoContrato.HorContratual.horario.Add do
      begin
        Dia := diSegundaFeira;
        CodHorContrat := '001';
      end;

      with EvtAltContratual.AltContratual.infoContrato.FiliacaoSindical.Add do
        CnpjSindTrab := '12345678901234';

      EvtAltContratual.AltContratual.infoContrato.AlvaraJudicial.NrProcJud := '543216';
      EvtAltContratual.AltContratual.infoContrato.servPubl.mtvAlter := maPromocao;

    end;
end;

procedure TFExemploEsocial.GerareSocial2210;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2210.Add do
  begin
    EvtCAT.id     := '1';
//    EvtCAT.Versao := '2.0';

    EvtCAT.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtCAT.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtCAT.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtCAT.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtCAT.IdeEvento.VerProc  := '1.0';

    EvtCAT.IdeRegistrador.tpRegistrador := tpTpRegistrador(0);
    EvtCAT.IdeRegistrador.TpInsc        := tpTpInsc(1);
    EvtCAT.IdeRegistrador.NrInsc        := '12345678901234';

    EvtCAT.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtCAT.IdeEmpregador.NrInsc := '12345678901234';

    EvtCAT.IdeTrabalhador.cpfTrab := '12345678901';
    EvtCAT.IdeTrabalhador.nisTrab := '12345678901';

    EvtCAT.Cat.dtAcid := Now;
    EvtCAT.Cat.TpAcid := '1.0.01';
    EvtCAT.Cat.hrAcid := '1200';

    EvtCAT.Cat.hrsTrabAntesAcid := '0400';
    EvtCAT.Cat.tpCat            := tpTpCat(0);
    EvtCAT.Cat.indCatObito      := tpNao;
    EvtCAT.Cat.dtOBito          := now;
    EvtCAT.Cat.indComunPolicia  := tpSim;
    EvtCAT.Cat.codSitGeradora   := 200004300;
    EvtCAT.Cat.iniciatCAT       := tpIniciatCAT(1);
    EvtCAT.Cat.observacao       := 'Teste';

    EvtCAT.Cat.LocalAcidente.tpLocal       := tpTpLocal(1);
    EvtCAT.Cat.LocalAcidente.dscLocal      := 'Local Teste';
    EvtCAT.Cat.LocalAcidente.dscLograd     := 'Logradouro Teste';
    EvtCAT.Cat.LocalAcidente.nrLograd      := '111';
    EvtCAT.Cat.LocalAcidente.codMunic      := 123;
    EvtCAT.Cat.LocalAcidente.uf            := tpuf(ufPR);
    EvtCAT.Cat.LocalAcidente.cnpjLocalAcid := '12345678901234';
    EvtCAT.Cat.LocalAcidente.pais          := '008';
    EvtCAT.Cat.LocalAcidente.codPostal     := '6546';

    with EvtCAT.Cat.ParteAtingida.Add do
    begin
      codParteAting := 753030000;
      lateralidade  := tpLateralidade(1);
    end;

    with EvtCAT.Cat.ParteAtingida.Add do
    begin
      codParteAting := 753070700;
      lateralidade  := tpLateralidade(2);
    end;

    with EvtCAT.Cat.ParteAtingida.Add do
    begin
      codParteAting := 753510200;
      lateralidade  := tpLateralidade(3);
    end;

    with EvtCAT.Cat.AgenteCausador.Add do
    begin
      codAgntCausador := 302010300;
    end;

    with EvtCAT.Cat.AgenteCausador.Add do
    begin
      codAgntCausador := 302010600;
    end;

    with EvtCAT.Cat.AgenteCausador.Add do
    begin
      codAgntCausador := 302050500;
    end;

    EvtCAT.Cat.Atestado.codCNES       := '1234567';
    EvtCAT.Cat.Atestado.dtAtendimento := now;
    EvtCAT.Cat.Atestado.hrAtendimento := '1330';
    EvtCAT.Cat.Atestado.indInternacao := tpSim;
    EvtCAT.Cat.Atestado.durTrat       := 5;
    EvtCAT.Cat.Atestado.indAfast      := tpSim;
    EvtCAT.Cat.Atestado.dscLesao      := 1;
    EvtCAT.Cat.Atestado.dscCompLesao  := 'Descricao complementar';
    EvtCAT.Cat.Atestado.diagProvavel  := 'Diagnostico teste';
    EvtCAT.Cat.Atestado.codCID        := '1234';
    EvtCAT.Cat.Atestado.observacao    := 'Observação teste';

    EvtCAT.Cat.Atestado.Emitente.nmEmit := 'Emitente Teste';
    EvtCAT.Cat.Atestado.Emitente.ideOC  := tpIdeOC(1);
    EvtCAT.Cat.Atestado.Emitente.nrOc   := '456123';
    EvtCAT.Cat.Atestado.Emitente.ufOC   := tpuf(ufPR);

    EvtCAT.Cat.CatOrigem.dtCatOrig := now;
    EvtCAT.Cat.CatOrigem.nrCatOrig := '123456';
  end;
end;

procedure TFExemploEsocial.GerareSocial2220;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2220.Add do
  begin
    EvtASO.id     := '1';
//    EvtASO.Versao := '2.0';

    EvtASO.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtASO.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtASO.IdeEvento.TpAmb    := TpTpAmb(1);
    EvtASO.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtASO.IdeEvento.VerProc  := '1.0';

    EvtASO.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtASO.IdeEmpregador.NrInsc := '12345678901234';

    EvtASO.IdeVinculo.cpfTrab   := '12345678901';
    EvtASO.IdeVinculo.nisTrab   := '12345678901';
    EvtASO.IdeVinculo.matricula := '5000';

    EvtASO.Aso.DtAso  := Date;
    EvtASO.Aso.tpAso  := tpTpAso(1);
    EvtASO.Aso.ResAso := tpResAso(1);

    with EvtASO.Aso.Exame.Add do
    begin
      DtExm := Date;
      procRealizado := 123;
      obsProc := 'observação do procedimento realizado';
      interprExm := tpInterprExm(0);
      ordExame := tpOrdExame(0);
      dtIniMonit := now;
      dtFimMonit := now;
      indResult := tpIndResult(1);

      RespMonit.NisResp      := '12345678901';
      RespMonit.NrConsClasse := '7893';
      RespMonit.UfConsClasse := tpuf(ufPR);
    end;

    with EvtASO.Aso.Exame.Add do
    begin
      DtExm := Date + 1;
      procRealizado := 456;
      obsProc := 'observação do procedimento realizado';
      ordExame := tpOrdExame(0);
      dtIniMonit := now;
      dtFimMonit := now;
      indResult := tpIndResult(1);

      RespMonit.NisResp      := '12345678901';
      RespMonit.NrConsClasse := '7893';
      RespMonit.UfConsClasse := tpuf(ufPR);
    end;

    EvtASO.Aso.IdeServSaude.CodCNES          := '9876541';
    EvtASO.Aso.IdeServSaude.FrmCtt           := 'Telefone: 32200000';
    EvtASO.Aso.IdeServSaude.Email            := 'teste@teste.com';
    EvtASO.Aso.IdeServSaude.Medico.NmMed     := 'MEDICO TESTE';
    EvtASO.Aso.IdeServSaude.Medico.CRM.NrCRM := '88888888';
    EvtASO.Aso.IdeServSaude.Medico.CRM.UfCRM := tpuf(ufPR);
  end;
end;

procedure TFExemploEsocial.GerareSocial2230;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2230.Add do
    begin
      EvtAfastTemp.id := '1';
//      EvtAfastTemp.versao := '2.0';

      EvtAfastTemp.IdeEvento.indRetif := ireOriginal;
//    EvtAfastTemp.IdeEvento.NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif=ireRetificacao
      EvtAfastTemp.IdeEvento.TpAmb := taProducaoRestritaDadosFicticios;
      EvtAfastTemp.IdeEvento.ProcEmi := peAplicEmpregador;
      EvtAfastTemp.IdeEvento.VerProc := '1.0';

      EvtAfastTemp.IdeEmpregador.TpInsc := tiCNPJ;
      EvtAfastTemp.IdeEmpregador.NrInsc := '12345678901234';

      EvtAfastTemp.IdeVinculo.cpfTrab := '12345678901';
      EvtAfastTemp.IdeVinculo.nisTrab := '12345678901';
      EvtAfastTemp.IdeVinculo.matricula := 'A123';

      EvtAfastTemp.infoAfastamento.iniAfastamento.DtIniAfast := now;
      EvtAfastTemp.infoAfastamento.iniAfastamento.codMotAfast := '01';
      EvtAfastTemp.infoAfastamento.iniAfastamento.infoMesmoMtv := tpNao;
      EvtAfastTemp.infoAfastamento.iniAfastamento.tpAcidTransito := tpatOutros;
      EvtAfastTemp.infoAfastamento.iniAfastamento.Observacao := 'Campo opcional, salvo quando codMotAfast=21 aí é obrigatória';

      with EvtAfastTemp.infoAfastamento.iniAfastamento.infoAtestado.Add do
      begin
        codCID := '1234';
        qtDiasAfast := 13;

        Emitente.nmEmit := 'João das Neves';
        Emitente.ideOC := idCRM;
        Emitente.nrOc := '3690';
        Emitente.ufOC := ufPR;
      end;

      //infoCessao opcional usado para afastamento por cessão de funcionário. Ex.: Orgão Público, Sindicatos, etc...
      EvtAfastTemp.infoAfastamento.iniAfastamento.infoCessao.cnpjCess := '78945612303216';
      EvtAfastTemp.infoAfastamento.iniAfastamento.infoCessao.infOnus := ocCessionario;

      //infoMandSind opcional para cessão de funcionario para mandato sindical
      EvtAfastTemp.infoAfastamento.iniAfastamento.infoMandSind.cnpjSind := '12345678901234';
      EvtAfastTemp.infoAfastamento.iniAfastamento.infoMandSind.infOnusRemun := orEmpregador;

      //Apenas alteração do MOTIVO de afastamento
      EvtAfastTemp.infoAfastamento.altAfastamento.dtAltMot := Date;
      EvtAfastTemp.infoAfastamento.altAfastamento.codMotAnt := '16';
      EvtAfastTemp.infoAfastamento.altAfastamento.codMotAfast := '15';
      EvtAfastTemp.infoAfastamento.altAfastamento.infoMesmoMtv := tpNao;
      EvtAfastTemp.infoAfastamento.altAfastamento.indEfRetroativo := tpSim;
      EvtAfastTemp.infoAfastamento.altAfastamento.origAlt := tpOrigemAltAfast(0);
      EvtAfastTemp.infoAfastamento.altAfastamento.nrProcJud := '231321321';

      EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.codCID := '0123';
      EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.qtdDiasAfast := 10;
      EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.nmEmit := 'Nome do emitente na alteração';
      EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.ideOC := tpIdeOC(0);
      EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.nrOc := '12313';
      EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.ufOC := ufSP;

      //informações de término do Afastamento
      EvtAfastTemp.infoAfastamento.fimAfastamento.dtTermAfast := now;
//      EvtAfastTemp.infoAfastamento.fimAfastamento.codMotAfast := '15';
//      EvtAfastTemp.infoAfastamento.fimAfastamento.infoMesmoMtv := tpNao;
    end;
end;

procedure TFExemploEsocial.GerareSocial2240;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2240.Add do
  begin
    EvtExpRisco.id     := 'Id123';
//      EvtExpRisco.Versao := '1.2';

    EvtExpRisco.ideEvento.indRetif := ireOriginal;
    EvtExpRisco.ideEvento.nrRecibo       := '654654865656';
    EvtExpRisco.ideEvento.TpAmb          := taProducaoRestritaDadosFicticios;
    EvtExpRisco.ideEvento.ProcEmi        := peAplicEmpregador;
    EvtExpRisco.ideEvento.VerProc        := '1.0';

    EvtExpRisco.ideEmpregador.TpInsc := tiCNPJ;
    EvtExpRisco.ideEmpregador.NrInsc := '12345678901234';

    EvtExpRisco.IdeVinculo.CpfTrab   := '12345678901';
    EvtExpRisco.IdeVinculo.NisTrab   := '12345678901';
    EvtExpRisco.IdeVinculo.Matricula := '564545';

    EvtExpRisco.infoExpRisco.iniExpRisco.dtCondicao := Date;

    with EvtExpRisco.infoExpRisco.iniExpRisco.InfoAmb.Add do
    begin
      codAmb := '654';
      InfoAtiv.dscAtivDes := 'dscAtivDes';
      with FatRisco.Add do
      begin
        codFatRis := '1234567890';
        intConc := 'N/A';
        tecMedicao := 'Técnica de medição';

        epcEpi.utilizEPC := uEPCUtilizado;
        epcEpi.utilizEPI := uEPIUtilizado;

        with epcEpi.epc.add do
        begin
          dscEpc := 'Descrição do EPC 1';
          eficEpc := tpSim;
        end;

        with epcEpi.epi.add do
        begin
          caEPI := '321654';
          eficEpi := tpSim;
          medProtecao := tpSim;
          condFuncto := tpSim;
          przValid := tpSim;
          periodicTroca := tpSim;
          higienizacao := tpSim;
        end;
      end;
    end;


    //alteração das informações de condições de ambiente de trabalho, opcional
    EvtExpRisco.infoExpRisco.altExpRisco.dtCondicao := Date;

    with EvtExpRisco.infoExpRisco.altExpRisco.InfoAmb.Add do
    begin
      codAmb := '654';
      InfoAtiv.dscAtivDes := 'dscAtivDes';
      with FatRisco.Add do
      begin
        codFatRis := '1234567890';
        intConc := 'N/A';
        tecMedicao := 'Técnica de medição';

        epcEpi.utilizEPC := uEPCUtilizado;
        epcEpi.utilizEPI := uEPIUtilizado;

        with epcEpi.epc.add do
        begin
          dscEpc := 'Descrição do EPC 2';
          eficEpc := tpSim;
        end;

        with epcEpi.epi.Add do
        begin
          caEPI := '321654';
          eficEpi := tpSim;
          medProtecao := tpSim;
          condFuncto := tpSim;
          przValid := tpSim;
          periodicTroca := tpSim;
          higienizacao := tpSim;
        end;
      end;
    end;

    //fimExpRisco - opcional, informar quando o trabalhador não se sujeitar mais as condições de ambiente informadas anteriormente
    EvtExpRisco.infoExpRisco.fimExpRisco.dtFimCondicao := Date;

    with EvtExpRisco.infoExpRisco.fimExpRisco.infoAmb.Add do
    begin
      codAmb := '897654987';
    end;

    with EvtExpRisco.infoExpRisco.respReg.add do
    begin
      dtIni := now;
      dtFim := now;
      nisResp := '12345678901';
      nrOc := '51561561';
      ufOC := ufSP;
    end;

    with EvtExpRisco.infoExpRisco.respReg.add do
    begin
      dtIni := now;
      dtFim := now;
      nisResp := '12345678901';
      nrOc := '51561561';
      ufOC := ufSP;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2241;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2241.Add do
    begin
      EvtInsApo.id     := '1';
//      EvtInsApo.Versao := '1.2';

      EvtInsApo.ideEvento.indRetif := ireOriginal;
      EvtInsApo.ideEvento.nrRecibo       := '654654865656';
      EvtInsApo.ideEvento.TpAmb          := taProducao;
      EvtInsApo.ideEvento.ProcEmi        := peAplicEmpregador;
      EvtInsApo.ideEvento.VerProc        := '1.0';

      EvtInsApo.ideEmpregador.TpInsc := tiCNPJ;
      EvtInsApo.ideEmpregador.NrInsc := '12345678901234';

      EvtInsApo.IdeVinculo.CpfTrab   := '12345678901';
      EvtInsApo.IdeVinculo.NisTrab   := '12345678901';
      EvtInsApo.IdeVinculo.Matricula := '564545';

      //InsalPeric - Informações de insalubridade e periculosidade
      EvtInsApo.InsalPeric.iniInsalPeric.DtiniCondicao := Date -60;
      with EvtInsApo.InsalPeric.iniInsalPeric.InfoAmb.Add do
      begin
        codAmb := '654';
        InfoAtiv.dscAtivDes := 'dscAtivDes';
        with FatRisco.Add do
          codFatRis := '1234567890';
      end;

      //Opcional - usado para alterações nas condições de trabalho previamente informadas
      //so sera enviado posteriormente quando for alterar um registro
      (*EvtInsApo.InsalPeric.altInsalPeric.DtaltCondicao := Date;
      with EvtInsApo.InsalPeric.altInsalPeric.InfoAmb.Add do
        begin
          codAmb := '456';
          InfoAtiv.dscAtivDes := 'dscAtivDes';
          with FatRisco.Add do
            begin
              codFatRis := '321';
              intConc := 'N/A';
              tecMedicao := 'Técnica de medição';
            end;
        end;*)

      //Opcional - usado quando cessarem as condições de trabalho previamente informadas
//      EvtInsApo.InsalPeric.fimInsalPeric.DtfimCondicao := Date;
//      EvtInsApo.InsalPeric.fimInsalPeric.InfoAmb.codAmb := '123456';

      //AposentEsp - Infomações de condições que ensejam aposentadoria especial
      EvtInsApo.AposentEsp.iniAposentEsp.DtiniCondicao := Date - 60;
      with EvtInsApo.AposentEsp.iniAposentEsp.InfoAmb.Add do
      begin
        codAmb := '654';
        InfoAtiv.dscAtivDes := 'dscAtivDes';
        with FatRisco.Add do
          codFatRis := '1234567890';
      end;

      //Opcional - usado para alterações nas condições de trabalho previamente informadas
      //so sera enviado posteriormente quando for alterar um registro
      (*
      EvtInsApo.AposentEsp.altAposentEsp.DtaltCondicao := Date;
      with EvtInsApo.AposentEsp.altAposentEsp.InfoAmb.Add do
        begin
          codAmb := '456';
          InfoAtiv.dscAtivDes := 'dscAtivDes';
          with FatRisco.Add do
            begin
              codFatRis := '321';
              intConc := 'N/A';
              tecMedicao := 'Técnica de medição';
            end;
        end;
      *)

      //Opcional - usado quando cessarem as condições de trabalho previamente informadas
//      EvtInsApo.AposentEsp.fimAposentEsp.DtfimCondicao := Date;
//      EvtInsApo.AposentEsp.fimAposentEsp.InfoAmb.codAmb := '654321';
    end;
end;


procedure TFExemploEsocial.GerareSocial2250;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2250.Add do
  begin
    EvtAvPrevio.id     := '1';
//    EvtAvPrevio.Versao := '2.0';

    EvtAvPrevio.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtAvPrevio.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAvPrevio.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtAvPrevio.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtAvPrevio.IdeEvento.VerProc  := '1.0';

    EvtAvPrevio.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtAvPrevio.IdeEmpregador.NrInsc := '12345678901234';

    EvtAvPrevio.IdeVinculo.cpfTrab   := '12345678901';
    EvtAvPrevio.IdeVinculo.nisTrab   := '12345678901';
    EvtAvPrevio.IdeVinculo.matricula := '123456';

    //aviso
    if cbAviso.ItemIndex = 0 then
    begin
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.dtAvPrv      := Now;
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.dtPrevDeslig := Now + 30;
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.tpAvPrevio   := tpTpAvPrevio(0);
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.observacao   := 'Observacao aviso previo';
    end
    else //cancelamento aviso
    begin
      EvtAvPrevio.InfoAvPrevio.CancAvPrevio.dtCancAvPrv     := Now;
      EvtAvPrevio.InfoAvPrevio.CancAvPrevio.mtvCancAvPrevio := tpMtvCancAvPrevio(0);
      EvtAvPrevio.InfoAvPrevio.CancAvPrevio.observacao      := 'Observacao cancelamento aviso previo';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2298;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2298.Add do
  begin
    EvtReintegr.id     := '1';
//    EvtReintegr.Versao := '2.0';

    EvtReintegr.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtReintegr.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtReintegr.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtReintegr.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtReintegr.IdeEvento.VerProc  := '1.0';

    EvtReintegr.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtReintegr.IdeEmpregador.NrInsc := '0123456789';

    EvtReintegr.IdeVinculo.cpfTrab   := '12345678901';
    EvtReintegr.IdeVinculo.nisTrab   := '88888888888';
    EvtReintegr.IdeVinculo.matricula := '123456';

    EvtReintegr.InfoReintegr.tpReint       := tpTpReint(0);
    EvtReintegr.InfoReintegr.nrProcJud     := '999999999';
    EvtReintegr.InfoReintegr.dtEfetRetorno := Now + 20;
    EvtReintegr.InfoReintegr.dtEfeito      := Now;
    EvtReintegr.InfoReintegr.indPagtoJuizo := tpSim;
  end;
end;

procedure TFExemploEsocial.GerareSocial2299;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2299.Add do
  begin
    EvtDeslig.id     := '1';
//      EvtDeslig.Versao := '2.0';

    EvtDeslig.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtDeslig.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtDeslig.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtDeslig.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtDeslig.IdeEvento.VerProc  := '1.0';

    EvtDeslig.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtDeslig.IdeEmpregador.NrInsc := '0123456789';

    EvtDeslig.IdeVinculo.cpfTrab   := '33333333303';
    EvtDeslig.IdeVinculo.nisTrab   := '11111111111';
    EvtDeslig.IdeVinculo.matricula := '123456';

    EvtDeslig.InfoDeslig.mtvDeslig := '02';
    EvtDeslig.InfoDeslig.dtDeslig  := Date;
    EvtDeslig.InfoDeslig.indPagtoAPI := tpSim;
    EvtDeslig.InfoDeslig.dtProjFimAPI := now;
    EvtDeslig.InfoDeslig.pensAlim := paPercentualeValordePensaoAlimenticia;
    EvtDeslig.InfoDeslig.percAliment := 25.5;
    EvtDeslig.InfoDeslig.vrAlim := 1985.65;

    //Certidão de óbito apenas em caso de morte quando o mtvDeslig for 09 ou 10
    EvtDeslig.InfoDeslig.nrCertObito := '0123456789';

    //numero do processo que decidiu o desligamento mtvdeslig = 17
    EvtDeslig.InfoDeslig.nrProcTrab := '9632587410';

    EvtDeslig.InfoDeslig.indCumprParc := cpaCumprimentoTotal;

    //Obsercação opcional
    EvtDeslig.InfoDeslig.Observacao := 'Anotações relevantes sobre o desligamento que não tenham campo próprio';

    EvtDeslig.InfoDeslig.SucessaoVinc.cnpjEmpregAnt := '12345678912345';//Corrigir nome do campo ou mudar classe.

    with EvtDeslig.InfoDeslig.VerbasResc.dmDev.add do
    begin
      ideDmDev := '1234567890';
      with infoPerApur.ideEstabLot.add do
      begin
        tpInsc := tiCNPJ;
        nrInsc := '12345678901234';
        codLotacao := 'A1234';
        with detVerbas.add do
        begin
          codRubr := 'Pg123';
          ideTabRubr := 'A01';
          qtdRubr := 2;
          fatorRubr := 200.65;
          vrUnit := 152.35;
          vrRubr := 304.70;
        end;
        with infoSaudeColet.detOper.add do
        begin
          cnpjOper := '74563215000195';
          regANS := '123456';
          vrPgTit := 150.65;
          with detPlano.add do
          begin
            cpfDep := '11111111111';
            nmDep := 'Nome do dependente';
            dtNascto := now;
            vlrPgDep := 150.84;
          end;
        end;
        infoAgNocivo.grauExp := ge1;
        infoSimples.indSimples := idsIntegralmente;
      end;

      with infoPerAnt.ideADC.add do
      begin
        dtAcConv := now;
        tpAcConv := tacAcordoColTrab;
        dtEfAcConv := now;
        dsc := 'Detalhamento';
        with idePeriodo.add do
        begin
          perRef := '2017-05';
          with ideEstabLot.add do
          begin
            tpInsc := tiCNPJ;
            nrInsc := '12345678901234';
            codLotacao := 'A1234';
            with detVerbas.add do
            begin
              codRubr := 'Pg124';
              ideTabRubr := 'A01';
              qtdRubr := 0.4;
              fatorRubr := 200.65;
              vrUnit := 200.35;
              vrRubr := 1500.70;
            end;
            infoAgNocivo.grauExp := ge1;
            infoSimples.indSimples := idsIntegralmente;
          end;
        end;
      end;
    end;

    with EvtDeslig.InfoDeslig.VerbasResc.ProcJudTrab.Add do
    begin
      tpTrib := tptIRRF;
      nrProcJud := '0123654789';
      codSusp := 1235;
    end;

    EvtDeslig.InfoDeslig.VerbasResc.infoMV.indMV := tpIndMV(0);
    with EvtDeslig.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.add do
    begin
      tpInsc := tiCNPJ;
      nrInsc := '12345678901234';
      codCateg := 123;
      vlrRemunOE := 500.84;
    end;

    EvtDeslig.InfoDeslig.Quarentena.dtFimQuar := now;

    EvtDeslig.InfoDeslig.consigFGTS.idConsig := tpSim;
    EvtDeslig.InfoDeslig.consigFGTS.insConsig := '12345';
    EvtDeslig.InfoDeslig.consigFGTS.nrContr := '123456';
  end;
end;

procedure TFExemploEsocial.GerareSocial2300;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2300.Add do
    begin
      EvtTSVInicio.id := '1';
//      EvtTSVInicio.versao := '2.0';

      EvtTSVInicio.IdeEvento.TpAmb := taProducaoRestritaDadosFicticios;
      EvtTSVInicio.IdeEvento.ProcEmi := peAplicEmpregador;
      EvtTSVInicio.IdeEvento.VerProc := '1.0';

      EvtTSVInicio.IdeEmpregador.TpInsc := tiCNPJ;
      EvtTSVInicio.IdeEmpregador.NrInsc := '12345678987654';

      EvtTSVInicio.Trabalhador.CpfTrab := '98765432123';
      EvtTSVInicio.Trabalhador.NisTrab := '54789632145';
      EvtTSVInicio.Trabalhador.NmTrab  := 'João das Neve';
      EvtTSVInicio.Trabalhador.Sexo    := 'M';
      EvtTSVInicio.Trabalhador.RacaCor := 1;
      EvtTSVInicio.Trabalhador.EstCiv  := 1;
      EvtTSVInicio.Trabalhador.GrauInstr := '07';
      EvtTSVInicio.Trabalhador.nmSoc := 'Nome social';

      EvtTSVInicio.Trabalhador.Nascimento.dtNascto := Date;
      EvtTSVInicio.Trabalhador.Nascimento.codMunic := 4153623;
      EvtTSVInicio.Trabalhador.Nascimento.UF       := 'PR';
      EvtTSVInicio.Trabalhador.Nascimento.PaisNascto := '063';
      EvtTSVInicio.Trabalhador.Nascimento.PaisNac   := '105';
      EvtTSVInicio.Trabalhador.Nascimento.NmMae := 'Joana das Neve';
      EvtTSVInicio.Trabalhador.Nascimento.NmPai := 'Jose das Neve';

      EvtTSVInicio.Trabalhador.Documentos.CTPS.NrCtps := '01234567897';
      EvtTSVInicio.Trabalhador.Documentos.CTPS.SerieCtps := '00001';
      EvtTSVInicio.Trabalhador.Documentos.CTPS.UfCtps := 'PR';

      EvtTSVInicio.Trabalhador.Documentos.RIC.NrRic := '10203040506070';
      EvtTSVInicio.Trabalhador.Documentos.RIC.OrgaoEmissor := 'Orgão Emissor';
      EvtTSVInicio.Trabalhador.Documentos.RIC.DtExped := Date;

      EvtTSVInicio.Trabalhador.Documentos.RG.NrRg := '73062584';
      EvtTSVInicio.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
      EvtTSVInicio.Trabalhador.Documentos.RG.DtExped := Date;

      EvtTSVInicio.Trabalhador.Documentos.RNE.NrRne := '01020304050607';
      EvtTSVInicio.Trabalhador.Documentos.RNE.OrgaoEmissor := 'Orgao Emissor';
      EvtTSVInicio.Trabalhador.Documentos.RNE.DtExped := Date;

      EvtTSVInicio.Trabalhador.Documentos.OC.NrOc := '74108520963012';
      EvtTSVInicio.Trabalhador.Documentos.OC.OrgaoEmissor := 'CRProfissao';
      EvtTSVInicio.Trabalhador.Documentos.OC.DtExped := Date;
      EvtTSVInicio.Trabalhador.Documentos.OC.DtValid := Date;

      EvtTSVInicio.Trabalhador.Documentos.CNH.nrRegCnh     := '123654789632';
      EvtTSVInicio.Trabalhador.Documentos.CNH.DtExped      := Date;
      EvtTSVInicio.Trabalhador.Documentos.CNH.ufCnh        := ufPR;
      EvtTSVInicio.Trabalhador.Documentos.CNH.DtValid      := Date;
      EvtTSVInicio.Trabalhador.Documentos.CNH.dtPriHab     := Date;
      EvtTSVInicio.Trabalhador.Documentos.CNH.categoriaCnh := cnAB;

      EvtTSVInicio.Trabalhador.Endereco.Brasil.TpLograd    := 'R';
      EvtTSVInicio.Trabalhador.Endereco.Brasil.DscLograd   := 'Rua Parmenides';
      EvtTSVInicio.Trabalhador.Endereco.Brasil.NrLograd    := '123456';
      EvtTSVInicio.Trabalhador.Endereco.Brasil.Complemento := 'fundos';
      EvtTSVInicio.Trabalhador.Endereco.Brasil.Bairro      := 'Jd Filosofia';
      EvtTSVInicio.Trabalhador.Endereco.Brasil.Cep         := '88888888';
      EvtTSVInicio.Trabalhador.Endereco.Brasil.CodMunic    := 4141414;
      EvtTSVInicio.Trabalhador.Endereco.Brasil.UF          := ufPR;

      //Dados de trabalhador estrangeiro
      EvtTSVInicio.Trabalhador.Endereco.Exterior.PaisResid   := '063';
      EvtTSVInicio.Trabalhador.Endereco.Exterior.DscLograd   := 'St. Abbey Road';
      EvtTSVInicio.Trabalhador.Endereco.Exterior.NrLograd    := '123456';
      EvtTSVInicio.Trabalhador.Endereco.Exterior.Complemento := 'apto 010';
      EvtTSVInicio.Trabalhador.Endereco.Exterior.Bairro      := 'RubberSoul';
      EvtTSVInicio.Trabalhador.Endereco.Exterior.NmCid       := 'Buenos Aires';
      EvtTSVInicio.Trabalhador.Endereco.Exterior.CodPostal   := '987654';

      EvtTSVInicio.Trabalhador.TrabEstrangeiro.DtChegada        := Date;
      EvtTSVInicio.Trabalhador.TrabEstrangeiro.ClassTrabEstrang := ctVistoPermanente;
      EvtTSVInicio.Trabalhador.TrabEstrangeiro.CasadoBr         := 'S';
      EvtTSVInicio.Trabalhador.TrabEstrangeiro.FilhosBr         := 'N';

      //Dados de trabalhador com deficiencia
      EvtTSVInicio.Trabalhador.InfoDeficiencia.DefFisica      := tpNao;
      EvtTSVInicio.Trabalhador.InfoDeficiencia.DefVisual      := tpNao;
      EvtTSVInicio.Trabalhador.InfoDeficiencia.DefAuditiva    := tpNao;
      EvtTSVInicio.Trabalhador.InfoDeficiencia.DefMental      := tpNao;
      EvtTSVInicio.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
      EvtTSVInicio.Trabalhador.InfoDeficiencia.ReabReadap     := tpSim;
      EvtTSVInicio.Trabalhador.InfoDeficiencia.Observacao     := 'sem deficiencia';

      with EvtTSVInicio.Trabalhador.Dependente.Add do
        begin
          tpDep    := tdConjuge;
          nmDep    := 'Dependente 1';
          dtNascto := Date;
          cpfDep   := '99999999909';
          depIRRF  := tpSim;
          depSF    := tpNao;
        end;

      with EvtTSVInicio.Trabalhador.Dependente.Add do
        begin
          tpDep    := tdFilhoOuEnteadoAte21Anos;
          nmDep    := 'Dependente 2';
          dtNascto := Date;
          cpfDep   := '99999999909';
          depIRRF  := tpSim;
          depSF    := tpNao;
        end;

      EvtTSVInicio.Trabalhador.Contato.FonePrinc     := '91067240';
      EvtTSVInicio.Trabalhador.Contato.FoneAlternat  := '91067240';
      EvtTSVInicio.Trabalhador.Contato.EmailPrinc    := 'TESTE@email.com.br';
      EvtTSVInicio.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';

      EvtTSVInicio.infoTSVInicio.codCateg := 101;
      EvtTSVInicio.infoTSVInicio.dtInicio := Date;
      EvtTSVInicio.infoTSVInicio.natAtivididade := navUrbano;

      EvtTSVInicio.infoTSVInicio.infoComplementares.cargoFuncao.codCargo  := '001';
      EvtTSVInicio.infoTSVInicio.infoComplementares.cargoFuncao.codFuncao := '001';

      EvtTSVInicio.infoTSVInicio.infoComplementares.Remuneracao.VrSalFx    := 1200.00;
      EvtTSVInicio.infoTSVInicio.infoComplementares.Remuneracao.UndSalFixo := sfPorMes;
      EvtTSVInicio.infoTSVInicio.infoComplementares.Remuneracao.DscSalVar  := 'Comissão de 1,2% sobre a venda do mês';

      EvtTSVInicio.infoTSVInicio.infoComplementares.Fgts.OpcFGTS   := ofOptante;
      EvtTSVInicio.infoTSVInicio.infoComplementares.Fgts.DtOpcFGTS := Date;

      //dados da empresa de origem do dirigente sindical
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoDirSind.categOrig  := 111;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoDirSind.cnpjOrigem := '12345678901234';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoDirSind.dtAdmOrig  := Date;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoDirSind.matricOrig := 'A1234';

      //Informações de trabalhador cedido, devem ser preenchidas exclusivamente pelo cessionário
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.categOrig := 111;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.cnpjCednt := '12345678901234';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.matricCed := 'B4321';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.dtAdmCed  := Date;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.tpRegTrab := trCLT;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.tpRegPrev := rpRGPS;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoTrabCedido.infOnus   := ocCedente;

      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.natEstagio  := neObrigatiorio;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.nivEstagio  := nvSuperior;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.areaAtuacao := 'Direito';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.nrApol      := '123456';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.vlrBolsa    := 600.00;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.dtPrevTerm  := IncMonth(Date, 12);

      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino := '12345678901234';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nmRazao        := 'Nome da Instituição de Ensino';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.dscLograd      := 'R Pitagoras';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nrLograd       := '1618';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.bairro         := 'Bairro Educacional';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Cep            := '86086086';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.codMunic       := 4141414;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Uf             := ufPR;

      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg := '12345678901234';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.nmRazao       := 'Nome da empresa';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.dscLograd     := 'R Adam Smith';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.nrLograd      := '9999';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.bairro        := 'Bairro Empresarial';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.Cep           := '86086086';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.codMunic      := 4141414;
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.Uf            := ufPR;

      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor := '88888888801';
      EvtTSVInicio.infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.nmSuperv      := 'Pedro das Pedras';
    end;
end;

procedure TFExemploEsocial.GerareSocial2306;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2306.Add do
    begin
      EvtTSVAltContr.id := '1';
//      EvtTSVAltContr.versao := '2.0';

      EvtTSVAltContr.IdeEvento.TpAmb := taProducaoRestritaDadosFicticios;
      EvtTSVAltContr.IdeEvento.ProcEmi := peAplicEmpregador;
      EvtTSVAltContr.IdeEvento.VerProc := '1.0';

      EvtTSVAltContr.IdeEmpregador.TpInsc := tiCNPJ;
      EvtTSVAltContr.IdeEmpregador.NrInsc := '12345678987654';

      EvtTSVAltContr.IdeTrabSemVinc.cpfTrab := '12345678901';
      EvtTSVAltContr.IdeTrabSemVinc.nisTrab := '00000000000';
      EvtTSVAltContr.IdeTrabSemVinc.codCateg := 555;

      EvtTSVAltContr.infoTSVAlteracao.dtAlteracao    := Date;
      EvtTSVAltContr.infoTSVAlteracao.natAtivididade := navUrbano;

      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.cargoFuncao.codCargo  := '001';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.cargoFuncao.codFuncao := '001';

      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.Remuneracao.VrSalFx    := 1200.00;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.Remuneracao.UndSalFixo := sfPorMes;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.Remuneracao.DscSalVar  := 'Comissão de 1,2% sobre a venda do mês';

      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.natEstagio  := neObrigatiorio;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.nivEstagio  := nvSuperior;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.areaAtuacao := 'Direito';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.nrApol      := '123456';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.vlrBolsa    := 600.00;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.dtPrevTerm  := IncMonth(Date, 12);

      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino := '12345678998765';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nmRazao        := 'Nome da Instituição de Ensino';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.dscLograd      := 'R Pitagoras';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nrLograd       := '1618';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.bairro         := 'Bairro Educacional';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Cep            := '86086086';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.codMunic       := 4141414;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Uf             := ufPR;

      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg := '98765432145678';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.nmRazao       := 'Nome da empresa';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.dscLograd     := 'R Adam Smith';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.nrLograd      := '9999';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.bairro        := 'Bairro Empresarial';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.Cep           := '86086086';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.codMunic      := 4141414;
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.Uf            := ufPR;

      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor := '12345678901';
      EvtTSVAltContr.infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.nmSuperv      := 'Pedro das Pedras';
    end;
end;

procedure TFExemploEsocial.GerareSocial2399;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2399.Add do
  begin
     EvtTSVTermino.id     := '1';
//      EvtTSVTermino.Versao := '2.0';

    EvtTSVTermino.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtTSVTermino.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtTSVTermino.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtTSVTermino.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtTSVTermino.IdeEvento.VerProc  := '1.0';

    EvtTSVTermino.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtTSVTermino.IdeEmpregador.NrInsc := '0123456789';

    EvtTSVTermino.IdeTrabSemVInc.cpfTrab  := '12345678987';
    EvtTSVTermino.IdeTrabSemVInc.nisTrab  := '98765432123';
    EvtTSVTermino.IdeTrabSemVInc.codCateg := 111;

    EvtTSVTermino.InfoTSVTermino.dtTerm := Date;

    EvtTSVTermino.InfoTSVTermino.mtvDesligTSV := '02';

    with EvtTSVTermino.InfoTSVTermino.verbasResc.dmDev.Add do
    begin
      ideDmDev := '012345';
      with ideEstabLot.Add do
      begin
        tpInsc := tiCNPJ;
        nrInsc := '12345678987654';
        codLotacao := 'A1234';
        with detVerbas.Add do
        begin
          codRubr := 'Pg123';
          ideTabRubr := 'A01';
          qtdRubr := 2;
          fatorRubr := 50.25;
          vrUnit := 152.35;
          vrRubr := 304.70;
        end;
        with infoSaudeColet.detOper.add do
        begin
          cnpjOper := '89652048000195';
          regANS := '123456';
          vrPgTit := 1500.65;
          with detPlano.add do
          begin
            cpfDep := '55555555555';
            nmDep := 'Nome do Dependente';
            dtNascto := now;
            vlrPgDep := 125.36;
          end;
        end;
        infoAgNocivo.grauExp := ge1;
        infoSimples.indSimples := idsIntegralmente;
      end;
    end;
    with EvtTSVTermino.InfoTSVTermino.verbasResc.procJudTrab.add do
    begin
      TpTrib := tpTpTributo(0);
      nrProcJud := '123456789';
      codSusp := 123456;
    end;
    EvtTSVTermino.InfoTSVTermino.verbasResc.infoMV.indMV := tpIndMV(0);
    with EvtTSVTermino.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.add do
    begin
      tpInsc := tiCNPJ;
      nrInsc := '14236547000195';
      codCateg := 111;
      vlrRemunOE := 2500.12;
    end;

    EvtTSVTermino.InfoTSVTermino.quarentena.dtFimQuar := Date;
  end;
end;

procedure TFExemploEsocial.GerareSocial2400;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2400.Add do
  begin
    EvtCdBenPrRP.id     := '1';

    EvtCdBenPrRP.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtCdBenPrRP.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtCdBenPrRP.IdeEvento.TpAmb    := taProducaoRestritaDadosFicticios;
    EvtCdBenPrRP.IdeEvento.ProcEmi  := TpProcEmi(0);
    EvtCdBenPrRP.IdeEvento.VerProc  := '1.0';

    EvtCdBenPrRP.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtCdBenPrRP.IdeEmpregador.NrInsc := '0123456789';

    EvtCdBenPrRP.ideBenef.cpfBenef := '12345678910';
    EvtCdBenPrRP.ideBenef.nmBenefic := 'Nome do beneficiario';
//    EvtCdBenPrRP.ideBenef.dadosBenef.cpfBenef := '12345678910';
//    EvtCdBenPrRP.ideBenef.dadosBenef.nmBenefic := 'Nome do beneficiario';

    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.dtNascto := Date;;
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.codMunic := 4153623;
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.UF       := 'PR';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.PaisNascto := '063';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.PaisNac   := '105';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.NmMae := 'Joana das Neve';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.NmPai := 'Jose das Neve';

    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.TpLograd    := 'R';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.DscLograd   := 'Rua Parmenides';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.NrLograd    := '123456';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.Complemento := 'fundos';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.Bairro      := 'Jd Filosofia';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.Cep         := '88888888';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.CodMunic    := 4141414;
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.UF          := ufPR;

    //Dados de trabalhador estrangeiro
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.PaisResid   := '063';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.DscLograd   := 'St. Abbey Road';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.NrLograd    := '123456';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.Complemento := 'apto 010';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.Bairro      := 'RubberSoul';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.NmCid       := 'Buenos Aires';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.CodPostal   := '987654';

    EvtCdBenPrRP.infoBeneficio.tpPlanRP := prpPlanoPrevidenciarioOuUnico;

    EvtCdBenPrRP.infoBeneficio.iniBeneficio.tpBenef := 1;
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.nrBenefic := '3156189132131';
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.dtIniBenef := now;
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.vrBenef := 1500.32;
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.infoPenMorte.idQuota := '1521651651';
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.infoPenMorte.cpfInst := '12345678910';

    EvtCdBenPrRP.infoBeneficio.altBeneficio.tpBenef := 1;
    EvtCdBenPrRP.infoBeneficio.altBeneficio.nrBenefic := '3156189132131';
    EvtCdBenPrRP.infoBeneficio.altBeneficio.dtIniBenef := now;
    EvtCdBenPrRP.infoBeneficio.altBeneficio.vrBenef := 1500.32;
    EvtCdBenPrRP.infoBeneficio.altBeneficio.infoPenMorte.idQuota := '1521651651';
    EvtCdBenPrRP.infoBeneficio.altBeneficio.infoPenMorte.cpfInst := '12345678910';

    EvtCdBenPrRP.infoBeneficio.fimBeneficio.tpBenef := 1;
    EvtCdBenPrRP.infoBeneficio.fimBeneficio.nrBenefic := '3156189132131';
    EvtCdBenPrRP.infoBeneficio.fimBeneficio.dtFimBenef := now;
    EvtCdBenPrRP.infoBeneficio.fimBeneficio.mtvFim := 1;

  end;
end;

procedure TFExemploEsocial.GerareSocial3000;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S3000.Add do
  begin
    EvtExclusao.id     := '1';
//    EvtExclusao.Versao := '2.0';

    EvtExclusao.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
    EvtExclusao.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtExclusao.IdeEvento.VerProc := '1.0';

    EvtExclusao.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtExclusao.IdeEmpregador.NrInsc := '0123456789';

    EvtExclusao.InfoExclusao.tpEvento := TTipoEvento(teS2100);
    EvtExclusao.InfoExclusao.nrRecEvt := '12345789987654321';

    EvtExclusao.InfoExclusao.IdeTrabalhador.cpfTrab := '12345678950';
    EvtExclusao.InfoExclusao.IdeTrabalhador.nisTrab := '12345678901';

    EvtExclusao.InfoExclusao.IdeFolhaPagto.indApuracao := tpIndApuracao(0);
    EvtExclusao.InfoExclusao.IdeFolhaPagto.perApur     := '2015-05';
  end;
end;

procedure TFExemploEsocial.GerareSocial4000;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S4000.Add do
  begin
    EvtSolicTotal.id     := '1';
//    EvtSolicTotal.Versao := '2.0';

    EvtSolicTotal.IdeEvento.TpAmb   := taProducaoRestritaDadosFicticios;
    EvtSolicTotal.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtSolicTotal.IdeEvento.VerProc := '1.0';

    EvtSolicTotal.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtSolicTotal.IdeEmpregador.NrInsc := '0123456789';

    EvtSolicTotal.InfoSolicitacao.opcConsult := tpOpcConsult(1);
    EvtSolicTotal.InfoSolicitacao.perSolicit := '05-2015';
    EvtSolicTotal.InfoSolicitacao.cpfTrab := '12345678911';
  end;
end;

procedure TFExemploEsocial.btnGerarClick(Sender: TObject);
begin
  if (cbS1000.Checked) then
    GerareSocial1000;

  if (cbS1005.Checked) then
    GerareSocial1005;

  if (cbS1010.Checked) then
    GerareSocial1010;

  if (cbS1020.Checked) then
    GerareSocial1020;

  if (cbS1030.Checked) then
    GerareSocial1030;

  if (cbS1035.Checked) then
    GerareSocial1035;

  if (cbS1040.Checked) then
    GerareSocial1040;

  if (cbS1050.Checked) then
    GerareSocial1050;

  if (cbS1060.Checked) then
    GerareSocial1060;

  if (cbS1070.Checked) then
    GerareSocial1070;

  if (cbS1080.Checked) then
    GerareSocial1080;

  if (cbS1200.Checked) then
    GerareSocial1200;

  if (cbS1202.Checked) then
    GerareSocial1202;

  if (cbS1207.Checked) then
    GerareSocial1207;

  if (cbS1210.Checked) then
    GerareSocial1210;

//  if (cbS1220.Checked) then
//    GerareSocial1220;

  if (cbS1250.Checked) then
    GerareSocial1250;

  if (cbS1260.Checked) then
    GerareSocial1260;

  if (cbS1270.Checked) then
    GerareSocial1270;

  if (cbS1280.Checked) then
    GerareSocial1280;

  if (cbS1295.checked) then
    GerareSocial1295;

  if (cbS1298.Checked) then
    GerareSocial1298;

  if (cbS1299.Checked) then
    GerareSocial1299;

  if (cbS1300.Checked) then
    GerareSocial1300;

  if (cbS2100.Checked) then
    GerareSocial2100;

  if (cbS2190.Checked) then
    GerareSocial2190;

  if (cbS2200.Checked) then
    GerareSocial2200;

  if (cbS2205.Checked) then
    GerareSocial2205;

  if (cbS2206.Checked) then
    GerareSocial2206;

  if (cbS2210.Checked) then
    GerareSocial2210;

  if (cbS2220.Checked) then
    GerareSocial2220;

  if (cbS2230.Checked) then
    GerareSocial2230;

  if (cbS2240.Checked) then
    GerareSocial2240;

  if (cbS2241.Checked) then
    GerareSocial2241;

  if (cbS2250.Checked) then
    GerareSocial2250;

  if (cbS2298.Checked) then
    GerareSocial2298;

  if (cbS2299.Checked) then
    GerareSocial2299;

  if (cbS2300.Checked) then
    GerareSocial2300;

//  if (cbS2305.Checked) then
//    GerareSocial2305;

  if (cbS2306.Checked) then
    GerareSocial2306;

  if (cbS2399.Checked) then
    GerareSocial2399;

  if (cbS2400.Checked) then
    GerareSocial2400;

  if (cbS3000.Checked) then
    GerareSocial3000;

  if (cbS4000.Checked) then
    GerareSocial4000;

//  if (cbS4999.Checked) then
//    GerareSocial4999;

  ACBreSocial1.Eventos.GerarXMLs;
  ACBreSocial1.Eventos.SaveToFiles;
  ACBreSocial1.Eventos.Clear;

  ShowMessage( 'XML de Eventos Gerados com Sucesso!' );
end;

procedure TFExemploEsocial.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFExemploEsocial.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFExemploEsocial.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFExemploEsocial.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFExemploEsocial.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBreSocial1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TFExemploEsocial.sbtnCaminhoCertClick(Sender: TObject);
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

procedure TFExemploEsocial.sbtnListaCertClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    ACBreSocial1.SSL.LerCertificadosStore;
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

    for I := 0 to ACBreSocial1.SSL.ListaCertificados.Count-1 do
    begin
      with ACBreSocial1.SSL.ListaCertificados[I] do
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

procedure TFExemploEsocial.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBreSocial1.SSL.SelecionarCertificado;
end;

procedure TFExemploEsocial.btnValidadeDataClick(Sender: TObject);
begin
  ShowMessage( FormatDateBr(ACBreSocial1.SSL.CertDataVenc) );
end;

procedure TFExemploEsocial.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage( ACBreSocial1.SSL.CertNumeroSerie );
end;

procedure TFExemploEsocial.btnSubjectNameClick(Sender: TObject);
begin
  ShowMessage( ACBreSocial1.SSL.CertSubjectName + sLineBreak + sLineBreak +
               'Razão Social: '+ACBreSocial1.SSL.CertRazaoSocial);
end;

procedure TFExemploEsocial.btnCNPJClick(Sender: TObject);
begin
  ShowMessage( ACBreSocial1.SSL.CertCNPJ );
end;

procedure TFExemploEsocial.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage( ACBreSocial1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: '+ACBreSocial1.SSL.CertCertificadora);
end;

procedure TFExemploEsocial.btnSHA_RSAClick(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBreSocial1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
//  MemoResp.Lines.Add( Ahash );
//  pgRespostas.ActivePageIndex := 0;
end;

procedure TFExemploEsocial.btnHTTPSClick(Sender: TObject);
//var
//  Acao: String;
//  OldUseCert: Boolean;
begin
(*
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

  OldUseCert := ACBreSocial1.SSL.UseCertificateHTTP;
  ACBreSocial1.SSL.UseCertificateHTTP := False;
  try
    MemoResp.Lines.Text := ACBreSocial1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBreSocial1.SSL.UseCertificateHTTP := OldUseCert;
  end;
  pgRespostas.ActivePageIndex := 0;
  *)
end;

procedure TFExemploEsocial.btnX509Click(Sender: TObject);
begin
(*
  with ACBreSocial1.SSL do
  begin
     CarregarCertificadoPublico(MemoDados.Lines.Text);
     MemoResp.Lines.Add(CertIssuerName);
     MemoResp.Lines.Add(CertRazaoSocial);
     MemoResp.Lines.Add(CertCNPJ);
     MemoResp.Lines.Add(CertSubjectName);
     MemoResp.Lines.Add(CertNumeroSerie);
    pgRespostas.ActivePageIndex := 0;
  end;
*)  
end;

procedure TFExemploEsocial.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TFExemploEsocial.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TFExemploEsocial.sbPatheSocialClick(Sender: TObject);
begin
  PathClick(edtPatheSocial);
end;

procedure TFExemploEsocial.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TFExemploEsocial.ACBreSocial1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
// memoLog.Lines.Add(ALogLine);
end;

procedure TFExemploEsocial.ACBreSocial1StatusChange(Sender: TObject);
begin
(*
  case ACBreSocial1.Status of
    stIdleeSocial :
    begin
      if ( frmStatus <> nil ) then
        frmStatus.Hide;
    end;
    steSocialStatusServico :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    steSocialRecepcao :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando dados da eSocial...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    steSocialRetRecepcao :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Recebendo dados da eSocial...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    steSocialConsulta :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Consultando eSocial...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    steSocialEmail :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando Email...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    steSocialEvento :
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando Evento...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
  end;
  Application.ProcessMessages;
  *)
end;

procedure TFExemploEsocial.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TFExemploEsocial.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/35');
end;

procedure TFExemploEsocial.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TFExemploEsocial.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TFExemploEsocial.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TFExemploEsocial.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TFExemploEsocial.btnSalvarConfigClick(Sender: TObject);
begin
 GravarConfiguracao;
 LerConfiguracao;
end;

procedure TFExemploEsocial.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  K: TVersaoeSocial;
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

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoeSocial) to High(TVersaoeSocial) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoeSocial), integer(K) ) );
//  cbVersaoDF.Items[0] := 've240';
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  ACBreSocial1.Configuracoes.WebServices.Salvar := true;
end;

procedure TFExemploEsocial.PathClick(Sender: TObject);
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

procedure TFExemploEsocial.AtualizaSSLLibsCombo;
begin
 cbSSLLib.ItemIndex := Integer( ACBreSocial1.Configuracoes.Geral.SSLLib );
 cbCryptLib.ItemIndex := Integer( ACBreSocial1.Configuracoes.Geral.SSLCryptLib );
 cbHttpLib.ItemIndex := Integer( ACBreSocial1.Configuracoes.Geral.SSLHttpLib );
 cbXmlSignLib.ItemIndex := Integer( ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib );

 cbSSLType.Enabled := (ACBreSocial1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TFExemploEsocial.GravarConfiguracao;
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
    Ini.WriteBool(   'Arquivos', 'EmissaoPatheSocial', cbxEmissaoPatheSocial.Checked);
    Ini.WriteBool(   'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(   'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(   'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString( 'Arquivos', 'PatheSocial',      edtPatheSocial.Text);
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

  finally
    Ini.Free;
  end;
end;

procedure TFExemploEsocial.LerConfiguracao;
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

    ACBreSocial1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
    ACBreSocial1.Configuracoes.Certificados.Senha       := edtSenha.Text;
    ACBreSocial1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

    cbxAtualizarXML.Checked     := Ini.ReadBool(    'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(    'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString(  'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbVersaoDF.ItemIndex        := Ini.ReadInteger( 'Geral', 'VersaoDF',         0);
    ckSalvar.Checked            := Ini.ReadBool(    'Geral', 'Salvar',           True);
    cbxRetirarAcentos.Checked   := Ini.ReadBool(    'Geral', 'RetirarAcentos',   True);
    edtPathLogs.Text            := Ini.ReadString(  'Geral', 'PathSalvar',       PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text         := Ini.ReadString(  'Geral', 'PathSchemas',      PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoeSocial), integer(cbVersaoDF.ItemIndex) ));

    ACBreSocial1.SSL.DescarregarCertificado;

    with ACBreSocial1.Configuracoes.Geral do
    begin
      SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

      AtualizaSSLLibsCombo;

      ExibirErroSchema := cbxExibirErroSchema.Checked;
      RetirarAcentos   := cbxRetirarAcentos.Checked;
      FormatoAlerta    := edtFormatoAlerta.Text;
      VersaoDF         := TVersaoeSocial(cbVersaoDF.ItemIndex);
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

    with ACBreSocial1.Configuracoes.WebServices do
    begin
      UF         := cbUF.Text;
      Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
      Visualizar := cbxVisualizar.Checked;
      Salvar     := cbxSalvarSOAP.Checked;

      AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

      if NaoEstaVazio(edtAguardar.Text)then
        AguardarConsultaRet := ifThen(StrToInt(edtAguardar.Text)<1000, StrToInt(edtAguardar.Text)*1000, StrToInt(edtAguardar.Text))
      else
        edtAguardar.Text := IntToStr(AguardarConsultaRet);

      if NaoEstaVazio(edtTentativas.Text) then
        Tentativas := StrToInt(edtTentativas.Text)
      else
        edtTentativas.Text := IntToStr(Tentativas);

      if NaoEstaVazio(edtIntervalo.Text) then
        IntervaloTentativas := ifThen(StrToInt(edtIntervalo.Text)<1000,StrToInt(edtIntervalo.Text)*1000,StrToInt(edtIntervalo.Text))
      else
        edtIntervalo.Text := IntToStr(ACBreSocial1.Configuracoes.WebServices.IntervaloTentativas);

      TimeOut   := seTimeOut.Value;
      ProxyHost := edtProxyHost.Text;
      ProxyPort := edtProxyPorta.Text;
      ProxyUser := edtProxyUser.Text;
      ProxyPass := edtProxySenha.Text;
    end;

    ACBreSocial1.SSL.SSLType := TSSLType( cbSSLType.ItemIndex );

    cbxSalvarArqs.Checked         := Ini.ReadBool(   'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked        := Ini.ReadBool(   'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked    := Ini.ReadBool(   'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPatheSocial.Checked := Ini.ReadBool(   'Arquivos', 'EmissaoPatheSocial',   false);
    cbxSalvaPathEvento.Checked    := Ini.ReadBool(   'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked     := Ini.ReadBool(   'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked   := Ini.ReadBool(   'Arquivos', 'SepararPorModelo', false);
    edtPatheSocial.Text           := Ini.ReadString( 'Arquivos', 'PatheSocial',          '');
    edtPathEvento.Text            := Ini.ReadString( 'Arquivos', 'PathEvento',       '');

    with ACBreSocial1.Configuracoes.Arquivos do
    begin
      Salvar             := cbxSalvarArqs.Checked;
      SepararPorMes      := cbxPastaMensal.Checked;
      AdicionarLiteral   := cbxAdicionaLiteral.Checked;
      EmissaoPatheSocial := cbxEmissaoPatheSocial.Checked;
      SepararPorCNPJ     := cbxSepararPorCNPJ.Checked;
      SepararPorModelo   := cbxSepararPorModelo.Checked;
      PathSalvar         := edtPathLogs.Text;
      PathSchemas        := edtPathSchemas.Text;
      PatheSocial        := edtPatheSocial.Text;
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
  finally
     Ini.Free;
  end;
end;

end.

