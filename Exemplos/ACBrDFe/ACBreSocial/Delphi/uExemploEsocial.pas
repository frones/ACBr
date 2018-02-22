{ ****************************************************************************** }
{ Projeto: Componente ACBreSocial }
{ Biblioteca multiplataforma de componentes Delphi para envio dos eventos do }
{ eSocial - http://www.esocial.gov.br/ }
{  }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto }
{ Daniel Simoes de Almeida }
{ André Ferreira de Moraes }
{ }
{ Colaboradores nesse arquivo: }
{ }
{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr }
{ }
{ }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }
{ }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }
{ }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }
{ }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410 }
{ }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
  |*  - Doação do componente para o Projeto ACBr
  ****************************************************************************** }
{$I ACBr.inc}
unit uExemploEsocial;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, ShellAPI, StdCtrls, Buttons, DateUtils, Spin, ExtCtrls,
  ComCtrls,
  ACBrBase, ACBrDFe, ACBrUtil, ACBreSocial, ACBrMail, pcesConversaoeSocial,
  pcnConversao,
  ufrmStatus;

type
  TFExemploEsocial = class(TForm)
    ACBreSocial1: TACBreSocial;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    btnSalvarConfig: TBitBtn;
    pgcEventos: TPageControl;
    tbsEventosTabela: TTabSheet;
    tbsEventosPeriodicos: TTabSheet;
    tbsEventosNaoPeriodicos: TTabSheet;
    pgWebservice: TPageControl;
    tsResposta: TTabSheet;
    MemoResp: TMemo;
    tsXmlEnvio: TTabSheet;
    MemoXmlEnvio: TMemo;
    tsXmlRetorno: TTabSheet;
    MemoXmlRetorno: TMemo;
    tsFormaEnvio: TTabSheet;
    rdgGrupo: TRadioGroup;
    rdgOperacao: TRadioGroup;
    cbS1000: TCheckBox;
    cbS1005: TCheckBox;
    cbS1010: TCheckBox;
    cbS1020: TCheckBox;
    cbS1030: TCheckBox;
    cbS1035: TCheckBox;
    Checb_ZeraBase: TCheckBox;
    cbS1040: TCheckBox;
    cbS1050: TCheckBox;
    cbS1060: TCheckBox;
    cbS1070: TCheckBox;
    cbS1080: TCheckBox;
    cbS1200: TCheckBox;
    cbS1202: TCheckBox;
    cbS1207: TCheckBox;
    cbS1210: TCheckBox;
    cbS1250: TCheckBox;
    cbS1260: TCheckBox;
    cbS1300: TCheckBox;
    cbS1299: TCheckBox;
    cbS1298: TCheckBox;
    cbS1295: TCheckBox;
    cbS1280: TCheckBox;
    cbS1270: TCheckBox;
    cbS2190: TCheckBox;
    cbS2200: TCheckBox;
    cbS2205: TCheckBox;
    cbS2206: TCheckBox;
    cbS2210: TCheckBox;
    cbS2220: TCheckBox;
    cbS2230: TCheckBox;
    cbS2240: TCheckBox;
    cbS2241: TCheckBox;
    cbAviso: TComboBox;
    cbS2250: TCheckBox;
    cbS3000: TCheckBox;
    cbS2400: TCheckBox;
    cbS2399: TCheckBox;
    cbS2306: TCheckBox;
    cbS2300: TCheckBox;
    cbS2299: TCheckBox;
    cbS2298: TCheckBox;
    tsComandos: TTabSheet;
    btnGerar: TButton;
    btnConsultar: TButton;
    btnEnviar: TButton;
    chkClear: TCheckBox;
    tsLog: TTabSheet;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    Label3: TLabel;
    Label4: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
    cbxRetirarAcentos: TCheckBox;
    cbVersaoDF: TComboBox;
    edtPathSchemas: TEdit;
    TabSheet2: TTabSheet;
    Label6: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    GroupBox2: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label17: TLabel;
    sbtnGetCert: TSpeedButton;
    sbtnGetCert1: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    btnValidadeData: TButton;
    btnNumSerie: TButton;
    btnSubjectName: TButton;
    btnCNPJ: TButton;
    btnIssuerName: TButton;
    GroupBox3: TGroupBox;
    edtCalcHash: TEdit;
    btnSHA_RSA: TButton;
    cbAssinar: TCheckBox;
    btnHTTPS: TButton;
    btnX509: TButton;
    cbSSLLib: TComboBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbXmlSignLib: TComboBox;
    TabSheet5: TTabSheet;
    GroupBox4: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    cbxVisualizar: TCheckBox;
    rgTipoAmb: TRadioGroup;
    cbxSalvarSOAP: TCheckBox;
    seTimeOut: TSpinEdit;
    cbSSLType: TComboBox;
    GroupBox8: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    GroupBox9: TGroupBox;
    Label24: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    cbxAjustarAut: TCheckBox;
    edtTentativas: TEdit;
    edtIntervalo: TEdit;
    edtAguardar: TEdit;
    TabSheet7: TTabSheet;
    sbPatheSocial: TSpeedButton;
    Label28: TLabel;
    Label36: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPatheSocial: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPatheSocial: TEdit;
    edtPathEvento: TEdit;
    Label1: TLabel;
    gbDadosEmpresa: TGroupBox;
    Label2: TLabel;
    edtIdEmpregador: TEdit;
    Label5: TLabel;
    edtIdTransmissor: TEdit;
    tsDados: TTabSheet;
    MemoDados: TMemo;
    memoLog: TMemo;
    Label7: TLabel;
    cbTEmpregador: TComboBox;
    btnCarregarXML: TButton;
    btnCarregarINI: TButton;

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
    procedure btnEnviarClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure ACBreSocial1TransmissaoEventos(const AXML: String;
      ATipo: TeSocialEventos);
    procedure btnCarregarXMLClick(Sender: TObject);
    procedure btnCarregarINIClick(Sender: TObject);
  private
    { Private declarations }
    // procedures eventos de tabela
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

    // procedure eventos periodicos
    procedure GerareSocial1200;
    procedure GerareSocial1202;
    procedure GerareSocial1207;
    procedure GerareSocial1210;
    procedure GerareSocial1250;
    procedure GerareSocial1260;
    procedure GerareSocial1270;
    procedure GerareSocial1280;
    procedure GerareSocial1295;
    procedure GerareSocial1298;
    procedure GerareSocial1299;
    procedure GerareSocial1300;

    // procedures eventos não periódicos
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

    procedure AtualizaSSLLibsCombo;
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure LimparDocsPasta;
    procedure SelecionaEventos;
  public
    { Public declarations }
  end;

var
  FExemploEsocial: TFExemploEsocial;

implementation

uses
  TypInfo, synacode, blcksock, FileCtrl, StrUtils, Math,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL,
  ACBreSocialWebServices, unit2;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TFExemploEsocial }

procedure TFExemploEsocial.GerareSocial1000;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Iniciais.S1000.Add do
    begin
      evtInfoEmpregador.Sequencial := 0;

      evtInfoEmpregador.IdeEvento.TpAmb := taProducaoRestrita;
      // So tem na Versão 2.4.1
      // taProducao, taProducaoRestrita

      evtInfoEmpregador.IdeEvento.ProcEmi := TpProcEmi(0);
      evtInfoEmpregador.IdeEvento.VerProc := '1.0';

      evtInfoEmpregador.IdeEmpregador.TpInsc := tiCPF;
      evtInfoEmpregador.IdeEmpregador.NrInsc := '0123456789';

      evtInfoEmpregador.ModoLancamento := TModoLancamento(i);
      evtInfoEmpregador.InfoEmpregador.IdePeriodo.IniValid := '2015-05';
      evtInfoEmpregador.InfoEmpregador.IdePeriodo.FimValid := '2099-12';

      with evtInfoEmpregador.InfoEmpregador.InfoCadastro do
      begin
        if Checb_ZeraBase.Checked then
        Begin
          NmRazao := 'RemoverEmpregadorDaBaseDeDadosDaProducaoRestrita';
          ClassTrib := ct00;

        End
        else
        Begin
          NmRazao := 'Empresa Teste';
          ClassTrib := ct01;
        End;
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

      evtInfoEmpregador.InfoEmpregador.NovaValidade.IniValid := '2014-05';
      evtInfoEmpregador.InfoEmpregador.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1005;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Iniciais.S1005.Add do
    begin
      evtTabEstab.Sequencial := 0;

      evtTabEstab.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabEstab.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabEstab.IdeEvento.VerProc := '1.0';

      evtTabEstab.IdeEmpregador.TpInsc := tiCPF;
      evtTabEstab.IdeEmpregador.NrInsc := '0123456789';

      evtTabEstab.ModoLancamento := TModoLancamento(i);
      evtTabEstab.infoEstab.IdeEstab.TpInsc := tiCNPJ;
      evtTabEstab.infoEstab.IdeEstab.NrInsc := '012345678901234';
      evtTabEstab.infoEstab.IdeEstab.IniValid := '2015-05';
      evtTabEstab.infoEstab.IdeEstab.FimValid := '2099-12';

      evtTabEstab.infoEstab.DadosEstab.cnaePrep := '2015';

      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.AliqRat := arat1;
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.Fap := 1.5;
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.AliqRatAjust := 2.5;

      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.tpProc :=
        tpTpProc(1);
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.nrProc :=
        '20150512';
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.codSusp := '1';

      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.tpProc :=
        tpTpProc(1);
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.nrProc :=
        '20150512';
      evtTabEstab.infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.codSusp := '2';

      evtTabEstab.infoEstab.DadosEstab.infoCaepf.tpCaepf := tcContrIndividual;

      evtTabEstab.infoEstab.DadosEstab.infoObra.indSubstPatrObra :=
        tpIndSubstPatronalObra(1);

      evtTabEstab.infoEstab.DadosEstab.infoTrab.regPt := tpRegPt(3);

      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.contApr := tpContApr(2);
      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud := '20150612';
      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.contEntEd := tpSim;
      with evtTabEstab.infoEstab.DadosEstab.infoTrab.infoApr.infoEntEduc.Add do
        NrInsc := '0123456789';

      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoPCD.contPCD := tpContPCD(9);
      evtTabEstab.infoEstab.DadosEstab.infoTrab.infoPCD.nrProcJud := '20160131';

      evtTabEstab.infoEstab.NovaValidade.IniValid := '2014-05';
      evtTabEstab.infoEstab.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1010;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1010.Add do
    begin
      evtTabRubrica.Sequencial := 0;

      evtTabRubrica.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabRubrica.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabRubrica.IdeEvento.VerProc := '1.0';

      evtTabRubrica.IdeEmpregador.TpInsc := tiCPF;
      evtTabRubrica.IdeEmpregador.NrInsc := '0123456789';

      evtTabRubrica.ModoLancamento := TModoLancamento(i);

      evtTabRubrica.infoRubrica.IdeRubrica.CodRubr := '5445';
      evtTabRubrica.infoRubrica.IdeRubrica.ideTabRubr := '100000';
      evtTabRubrica.infoRubrica.IdeRubrica.IniValid := '2015-05';
      evtTabRubrica.infoRubrica.IdeRubrica.FimValid := '2015-06';

      evtTabRubrica.infoRubrica.DadosRubrica.dscRubr := 'Teste de S-1010';
      evtTabRubrica.infoRubrica.DadosRubrica.natRubr := 1022;
      evtTabRubrica.infoRubrica.DadosRubrica.tpRubr := tpTpRubr(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncCP := tpCodIncCP(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncIRRF := tpCodIncIRRF(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncFGTS := tpCodIncFGTS(1);
      evtTabRubrica.infoRubrica.DadosRubrica.codIncSIND := tpCodIncSIND(1);
      evtTabRubrica.infoRubrica.DadosRubrica.observacao := 'Rubrica Teste';

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoCP.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoCP.Add do
      begin
        nrProc := '1020';
        ExtDecisao := tpExtDecisao(1);
        codSusp := '1';
      end;

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoIRRF.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoIRRF.Add do
      begin
        nrProc := '1020';
        codSusp := '2';
      end;

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoFGTS.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoFGTS.Add do
      begin
        nrProc := '50740';
      end;

      evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoSIND.Clear;

      with evtTabRubrica.infoRubrica.DadosRubrica.IdeProcessoSIND.Add do
      begin
        nrProc := '50';
      end;

      if (evtTabRubrica.ModoLancamento = mlAlteracao) then
      begin
        evtTabRubrica.infoRubrica.NovaValidade.IniValid := '2015-05';
        evtTabRubrica.infoRubrica.NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1020;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1020.Add do
    begin
      evtTabLotacao.Sequencial := 0;

      evtTabLotacao.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabLotacao.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabLotacao.IdeEvento.VerProc := '1.0';

      evtTabLotacao.IdeEmpregador.TpInsc := tiCPF;
      evtTabLotacao.IdeEmpregador.NrInsc := '0123456789';

      evtTabLotacao.ModoLancamento := TModoLancamento(i);

      evtTabLotacao.infoLotacao.IdeLotacao.codLotacao := '300000';
      evtTabLotacao.infoLotacao.IdeLotacao.IniValid := '2015-06';
      evtTabLotacao.infoLotacao.IdeLotacao.FimValid := '2099-12';

      evtTabLotacao.infoLotacao.DadosLotacao.tpLotacao := '01';
      evtTabLotacao.infoLotacao.DadosLotacao.TpInsc := tiCAEPF;
      evtTabLotacao.infoLotacao.DadosLotacao.NrInsc := '6564646565';

      evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.Fpas := '515';
      evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.codTercs := '0015';
      evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.codTercsSusp := '0506';

      with evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.
        infoProcJudTerceiros.procJudTerceiro.Add do
      begin
        codTerc := '1111';
        nrProcJud := '1234567891239-1345';
        codSusp := '1';
      end;

      with evtTabLotacao.infoLotacao.DadosLotacao.fPasLotacao.
        infoProcJudTerceiros.procJudTerceiro.Add do
      begin
        codTerc := '2222';
        nrProcJud := '1234567891239-1345';
        codSusp := '2';
      end;

      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.tpInscContrat :=
        tpTpInscContratante(0);
      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.NrInscContrat :=
        '74563214500045';
      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.tpInscProp :=
        TpTpInscProp(0);
      evtTabLotacao.infoLotacao.DadosLotacao.InfoEmprParcial.nrInscProp :=
        '654234523416';

      evtTabLotacao.infoLotacao.NovaValidade.IniValid := '2015-06';
      evtTabLotacao.infoLotacao.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1030;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1030.Add do
    begin
      evtTabCargo.Sequencial := 0;

      evtTabCargo.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabCargo.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabCargo.IdeEvento.VerProc := '1.0';

      evtTabCargo.IdeEmpregador.TpInsc := tiCPF;
      evtTabCargo.IdeEmpregador.NrInsc := '0123456789';

      evtTabCargo.ModoLancamento := TModoLancamento(i);

      evtTabCargo.infoCargo.IdeCargo.CodCargo := '37';
      evtTabCargo.infoCargo.IdeCargo.IniValid := '2015-05';
      evtTabCargo.infoCargo.IdeCargo.FimValid := '2099-12';

      evtTabCargo.infoCargo.DadosCargo.nmCargo := 'Programador';
      evtTabCargo.infoCargo.DadosCargo.codCBO := '500000';

      evtTabCargo.infoCargo.DadosCargo.cargoPublico.acumCargo := tpAcumCargo(0);
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.contagemEsp :=
        tpContagemEsp(0);
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.dedicExcl := tpSimNao(0);

      evtTabCargo.infoCargo.DadosCargo.cargoPublico.leiCargo.nrLei := '11111';
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.leiCargo.dtLei := Now;
      evtTabCargo.infoCargo.DadosCargo.cargoPublico.leiCargo.sitCargo :=
        tpSitCargo(0);

      evtTabCargo.infoCargo.NovaValidade.IniValid := '2015-05';
      evtTabCargo.infoCargo.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1035;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1035.Add do
    begin
      evtTabCarreira.Sequencial := 0;

      evtTabCarreira.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabCarreira.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabCarreira.IdeEvento.VerProc := '1.0';

      evtTabCarreira.IdeEmpregador.TpInsc := tiCPF;
      evtTabCarreira.IdeEmpregador.NrInsc := '0123456789';

      evtTabCarreira.ModoLancamento := TModoLancamento(i);

      evtTabCarreira.InfoCarreira.ideCarreira.codCarreira := '1';
      evtTabCarreira.InfoCarreira.ideCarreira.IniValid := '2015-05';
      evtTabCarreira.InfoCarreira.ideCarreira.IniValid := '2099-12';

      evtTabCarreira.InfoCarreira.dadosCarreira.dscCarreira := 'Juiz';
      evtTabCarreira.InfoCarreira.dadosCarreira.leiCarr := 'lei89489/77';
      evtTabCarreira.InfoCarreira.dadosCarreira.dtLeiCarr := Now;
      evtTabCarreira.InfoCarreira.dadosCarreira.sitCarr := tpSitCarr(0);

      evtTabCarreira.InfoCarreira.NovaValidade.IniValid := '2015-05';
      evtTabCarreira.InfoCarreira.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1040;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1040.Add do
    begin
      evtTabFuncao.Sequencial := 0;

      evtTabFuncao.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabFuncao.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabFuncao.IdeEvento.VerProc := '1.0';

      evtTabFuncao.IdeEmpregador.TpInsc := tiCPF;
      evtTabFuncao.IdeEmpregador.NrInsc := '0123456789';

      evtTabFuncao.ModoLancamento := TModoLancamento(i);

      evtTabFuncao.InfoFuncao.IdeFuncao.CodFuncao := '1';
      evtTabFuncao.InfoFuncao.IdeFuncao.IniValid := '2015-05';
      evtTabFuncao.InfoFuncao.IdeFuncao.FimValid := '2099-12';

      evtTabFuncao.InfoFuncao.DadosFuncao.dscFuncao := 'PROGRAMADOR';
      evtTabFuncao.InfoFuncao.DadosFuncao.codCBO := '5000';

      evtTabFuncao.InfoFuncao.NovaValidade.IniValid := '2015-05';
      evtTabFuncao.InfoFuncao.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1050;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1050.Add do
    begin
      evtTabHorContratual.Sequencial := 0;

      evtTabHorContratual.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabHorContratual.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabHorContratual.IdeEvento.VerProc := '1.0';

      evtTabHorContratual.IdeEmpregador.TpInsc := tiCPF;
      evtTabHorContratual.IdeEmpregador.NrInsc := '0123456789';

      evtTabHorContratual.ModoLancamento := TModoLancamento(i);

      evtTabHorContratual.InfoHorContratual.ideHorContratual.
        codHorContrat := '1';
      evtTabHorContratual.InfoHorContratual.ideHorContratual.IniValid :=
        '2015-05';
      evtTabHorContratual.InfoHorContratual.ideHorContratual.FimValid :=
        '2099-12';

      evtTabHorContratual.InfoHorContratual.dadosHorContratual.hrEntr := '0800';
      evtTabHorContratual.InfoHorContratual.dadosHorContratual.hrSaida
        := '1800';
      evtTabHorContratual.InfoHorContratual.dadosHorContratual.
        durJornada := 525;
      evtTabHorContratual.InfoHorContratual.dadosHorContratual.perHorFlexivel :=
        tpSimNao(1);

      with evtTabHorContratual.InfoHorContratual.dadosHorContratual.
        horarioIntervalo.Add do
      begin
        tpInterv := tptpIntervalo(0);
        durInterv := 90;
        iniInterv := '1200';
        termInterv := '1330';
      end;

      with evtTabHorContratual.InfoHorContratual.dadosHorContratual.
        horarioIntervalo.Add do
      begin
        tpInterv := tptpIntervalo(1);
        durInterv := 15;
        iniInterv := '1645';
        termInterv := '1700';
      end;

      evtTabHorContratual.InfoHorContratual.NovaValidade.IniValid := '2015-05';
      evtTabHorContratual.InfoHorContratual.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1060;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1060.Add do
    begin
      EvtTabAmbiente.Sequencial := 0;

      EvtTabAmbiente.IdeEvento.TpAmb := taProducaoRestrita;
      EvtTabAmbiente.IdeEvento.ProcEmi := TpProcEmi(0);
      EvtTabAmbiente.IdeEvento.VerProc := '1.0';

      EvtTabAmbiente.IdeEmpregador.TpInsc := tiCPF;
      EvtTabAmbiente.IdeEmpregador.NrInsc := '0123456789';

      EvtTabAmbiente.ModoLancamento := TModoLancamento(i);

      EvtTabAmbiente.infoAmbiente.ideAmbiente.codAmb := '123456';
      EvtTabAmbiente.infoAmbiente.ideAmbiente.IniValid := '2015-05';
      EvtTabAmbiente.infoAmbiente.ideAmbiente.FimValid := '2099-12';

      EvtTabAmbiente.infoAmbiente.dadosAmbiente.dscAmb := 'DESCRICAO';
      EvtTabAmbiente.infoAmbiente.dadosAmbiente.localAmb := tpLocalAmb(0);
      EvtTabAmbiente.infoAmbiente.dadosAmbiente.TpInsc := tpTpInscAmbTab(0);
      EvtTabAmbiente.infoAmbiente.dadosAmbiente.NrInsc := '123456789';

      with EvtTabAmbiente.infoAmbiente.dadosAmbiente.fatorRisco.Add do
        codFatRis := '1111';

      with EvtTabAmbiente.infoAmbiente.dadosAmbiente.fatorRisco.Add do
        codFatRis := '2222';

      with EvtTabAmbiente.infoAmbiente.dadosAmbiente.fatorRisco.Add do
        codFatRis := '3333';

      EvtTabAmbiente.infoAmbiente.NovaValidade.IniValid := '2015-06';
      EvtTabAmbiente.infoAmbiente.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1070;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1070.Add do
    begin
      evtTabProcesso.Sequencial := 0;

      evtTabProcesso.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabProcesso.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabProcesso.IdeEvento.VerProc := '1.0';

      evtTabProcesso.IdeEmpregador.TpInsc := tiCPF;
      evtTabProcesso.IdeEmpregador.NrInsc := '0123456789';

      evtTabProcesso.ModoLancamento := TModoLancamento(i);

      evtTabProcesso.InfoProcesso.IdeProcesso.tpProc := tpTpProc(0);
      evtTabProcesso.InfoProcesso.IdeProcesso.nrProc := '5000';
      evtTabProcesso.InfoProcesso.IdeProcesso.IniValid := '2014-05';
      evtTabProcesso.InfoProcesso.IdeProcesso.FimValid := '2015-06';

      evtTabProcesso.InfoProcesso.DadosProc.IndAutoria := tpindAutoria(0);
      evtTabProcesso.InfoProcesso.DadosProc.indMatProc := tpIndMatProc(0);

      evtTabProcesso.InfoProcesso.DadosProc.DadosProcJud.UfVara := 'PR';
      evtTabProcesso.InfoProcesso.DadosProc.DadosProcJud.codMunic := 5075;
      evtTabProcesso.InfoProcesso.DadosProc.DadosProcJud.IdVara := '20';

      with evtTabProcesso.InfoProcesso.DadosProc.infoSusp.Add do
      begin
        codSusp := '1';
        indSusp := tpIndSusp(0);
        dtDecisao := Now;
        indDeposito := tpNao;
      end;

      evtTabProcesso.InfoProcesso.NovaValidade.IniValid := '2015-10';
      evtTabProcesso.InfoProcesso.NovaValidade.FimValid := '2016-10';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1080;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    with ACBreSocial1.Eventos.Tabelas.S1080.Add do
    begin
      evtTabOperPortuario.Sequencial := 0;

      evtTabOperPortuario.IdeEvento.TpAmb := taProducaoRestrita;
      evtTabOperPortuario.IdeEvento.ProcEmi := TpProcEmi(0);
      evtTabOperPortuario.IdeEvento.VerProc := '1.0';

      evtTabOperPortuario.IdeEmpregador.TpInsc := tiCPF;
      evtTabOperPortuario.IdeEmpregador.NrInsc := '0123456789';

      evtTabOperPortuario.ModoLancamento := TModoLancamento(i);

      evtTabOperPortuario.InfoOperPortuario.IdeOperPortuario.cnpjOpPortuario :=
        '29813268000156';
      evtTabOperPortuario.InfoOperPortuario.IdeOperPortuario.IniValid :=
        '2015-05';
      evtTabOperPortuario.InfoOperPortuario.IdeOperPortuario.FimValid :=
        '2099-12';

      evtTabOperPortuario.InfoOperPortuario.DadosOperPortuario.AliqRat := arat1;
      evtTabOperPortuario.InfoOperPortuario.DadosOperPortuario.Fap := 0.5;
      evtTabOperPortuario.InfoOperPortuario.DadosOperPortuario.
        AliqRatAjust := 5.5;

      evtTabOperPortuario.InfoOperPortuario.NovaValidade.IniValid := '2015-05';
      evtTabOperPortuario.InfoOperPortuario.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1200;
begin
  with ACBreSocial1.Eventos.Periodicos.S1200.Add do
  begin
    evtRemun.Sequencial := 0;

    evtRemun.IdeEvento.indRetif := ireOriginal;
    // evtRemun.ideEvento.NrRecibo  := '4564654'; Numero do recibo que será retificado.
    evtRemun.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    evtRemun.IdeEvento.perApur := '052015';
    evtRemun.IdeEvento.TpAmb := taProducaoRestrita;
    evtRemun.IdeEvento.ProcEmi := peAplicEmpregador;
    evtRemun.IdeEvento.VerProc := '1.0';

    evtRemun.IdeEmpregador.TpInsc := tiCNPJ;
    evtRemun.IdeEmpregador.NrInsc := '012345678987654';

    evtRemun.ideTrabalhador.CpfTrab := '01234567890';
    evtRemun.ideTrabalhador.NisTrab := '09876543210';

    evtRemun.ideTrabalhador.infoMV.indMV := imvDescontadaempregador;

    { Os Grupos abaixo são opcionais
      O grupo abaixocorresponde a funcionários que tenham dois empregos em empresas diferentes }
    with evtRemun.ideTrabalhador.infoMV.remunOutrEmpr.Add do
    begin
      TpInsc := tiCNPJ;
      NrInsc := '01234567890123';
      CodCateg := 222;
      vlrRemunOE := 1230.10;
    end;

    // o grupo abaixo corresponde apenas a trabalhadores cuja categoria não está sujeita ao evento de admissão
    // ou TSV-início
    evtRemun.ideTrabalhador.infoComplem.NmTrab := 'João das Neves';
    evtRemun.ideTrabalhador.infoComplem.DtNascto := date;
    evtRemun.ideTrabalhador.infoComplem.codCBO := '000001';
    evtRemun.ideTrabalhador.infoComplem.NatAtividade := navUrbano;
    evtRemun.ideTrabalhador.infoComplem.qtdDiasTrab := 10;

    evtRemun.ideTrabalhador.infoComplem.SucessaoVinc.cnpjEmpregAnt :=
      '12345678987654';
    evtRemun.ideTrabalhador.infoComplem.SucessaoVinc.MatricAnt := '123';
    evtRemun.ideTrabalhador.infoComplem.SucessaoVinc.DtAdm := Now;
    evtRemun.ideTrabalhador.infoComplem.SucessaoVinc.observacao :=
      'obs sucessao vinc';

    // os dados abaixo só devem ser informados em caso do processo existir e houver decisão que incida sobre as
    // contribuições
    with evtRemun.ideTrabalhador.procJudTrab.Add do
    begin
      tpTrib := tptPrevidenciaria;
      nrProcJud := '95135703320156150258';
      codSusp := 1;
    end;

    with evtRemun.dmDev.Add do
    begin
      ideDmDev := '1';
      CodCateg := 111;

      with infoPerApur.ideEstabLot.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '012345678987654';
        codLotacao := 'SACI54321';
        qtdDiasAv := 22;

        with remunPerApur.Add do
        begin
          Matricula := 'A1234';
          indSimples := idsIntegralmente;
          with itensRemun.Add do
          begin
            CodRubr := '987654';
            ideTabRubr := 'E380';
            qtdRubr := 100;
            fatorRubr := 50;
            vrUnit := 3296.35;
            vrRubr := 3330.30;
          end;

          with infoSaudeColet.detOper.Add do
          begin
            cnpjOper := '01234567898765';
            regANS := 'A1B2C3';
            vrPgTit := 1.50;
            with detPlano.Add do
            begin
              tpDep := tdConjuge;
              cpfDep := '01234567898';
              nmDep := 'José das Areias';
              DtNascto := date;
              vlrPgDep := 0.75;
            end;
          end;

          infoAgNocivo.grauExp := ge1;
        end;
      end;

      with infoPerAnt.ideADC.Add do
      begin
        dtAcConv := Now;
        tpAcConv := tacLegislacaoFederalEstadualMunicipalDistrital;
        dtEfAcConv := Now;
        compAcConv := '2017-01';
        dsc := 'Dissídio';
        with IdePeriodo.Add do
        begin
          perRef := '201504';
          with ideEstabLot.Add do
          begin
            TpInsc := tiCNPJ;
            NrInsc := '01234567898765';
            codLotacao := 'TESTE123';
            with remunPerAnt.Add do
            begin
              Matricula := 'A1234';
              indSimples := idsIntegralmente;
              with itensRemun.Add do
              begin
                CodRubr := '987654';
                ideTabRubr := 'E380';
                qtdRubr := 100;
                fatorRubr := 50;
                vrUnit := 3296.35;
                vrRubr := 3330.30;
              end;

              infoAgNocivo.grauExp := ge1;

            end;
          end;
        end;
      end;

      with infoTrabInterm.Add do
        codConv := '123456';

    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1202;
begin
  with ACBreSocial1.Eventos.Periodicos.S1202.Add do
  begin
    EvtRmnRPPS.Sequencial := 0;

    EvtRmnRPPS.IdeEvento.indRetif := ireOriginal;
    // evtRemun.ideEvento.NrRecibo  := '4564654'; Numero do recibo que será retificado.
    EvtRmnRPPS.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtRmnRPPS.IdeEvento.perApur := '052015';
    EvtRmnRPPS.IdeEvento.TpAmb := taProducaoRestrita;
    EvtRmnRPPS.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtRmnRPPS.IdeEvento.VerProc := '1.0';

    EvtRmnRPPS.IdeEmpregador.TpInsc := tiCNPJ;
    EvtRmnRPPS.IdeEmpregador.NrInsc := '012345678987654';

    EvtRmnRPPS.ideTrabalhador.CpfTrab := '01234567890';
    EvtRmnRPPS.ideTrabalhador.NisTrab := '09876543210';
    EvtRmnRPPS.ideTrabalhador.qtdDepFP := 0;

    // os dados abaixo só devem ser informados em caso do processo existir e houver decisão que incida sobre as  contribuições
    with EvtRmnRPPS.ideTrabalhador.procJudTrab.Add do
    begin
      tpTrib := tptPrevidenciaria;
      nrProcJud := '95135703320156150258';
      codSusp := 1;
    end;

    with EvtRmnRPPS.dmDev.Add do
    begin
      ideDmDev := '1';
      with infoPerApur.IdeEstab.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '012345678987654';
        with remunPerApur.Add do
        begin
          Matricula := 'A1234';
          CodCateg := 101;
          with itensRemun.Add do
          begin
            CodRubr := '987654';
            ideTabRubr := 'E380';
            qtdRubr := 100;
            fatorRubr := 50;
            vrUnit := 3296.35;
            vrRubr := 3330.30;
          end;

          with infoSaudeColet.detOper.Add do
          begin
            cnpjOper := '01234567898765';
            regANS := 'A1B2C3';
            vrPgTit := 1.50;
            with detPlano.Add do
            begin
              tpDep := tdConjuge;
              cpfDep := '01234567898';
              DtNascto := Now;
              nmDep := 'José das Areias';
              vlrPgDep := 0.75;
            end;
          end;
        end;
      end;

      with infoPerAnt.ideADC.Add do
      begin
        dtLei := Now;
        nrLei := '321321/2017';
        dtEf := Now;
        with IdePeriodo.Add do
        begin
          perRef := '2015-03';
          with IdeEstab.Add do
          begin
            TpInsc := tiCNPJ;
            NrInsc := '01234567898765';
            with remunPerAnt.Add do
            begin
              Matricula := 'A1234';
              CodCateg := 101;
              with itensRemun.Add do
              begin
                CodRubr := '987654';
                ideTabRubr := 'E380';
                qtdRubr := 100;
                fatorRubr := 50;
                vrUnit := 3296.35;
                vrRubr := 3330.30;
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
    evtBenPrRP.Sequencial := 0;

    evtBenPrRP.IdeEvento.indRetif := tpIndRetificacao(0);
    evtBenPrRP.IdeEvento.NrRecibo := '65.5454.987798798798';
    evtBenPrRP.IdeEvento.IndApuracao := iapuMensal;
    evtBenPrRP.IdeEvento.perApur := '2017-05';
    evtBenPrRP.IdeEvento.TpAmb := taProducaoRestrita;
    evtBenPrRP.IdeEvento.ProcEmi := TpProcEmi(0);
    evtBenPrRP.IdeEvento.VerProc := '1.0';

    evtBenPrRP.IdeEmpregador.TpInsc := tiCPF;
    evtBenPrRP.IdeEmpregador.NrInsc := '0123456789';

    evtBenPrRP.ideBenef.cpfBenef := '88888888888';

    with evtBenPrRP.dmDev.Add do
    begin
      tpBenef := 01;
      nrBenefic := '3132132';
      ideDmDev := '1';
      with itens.Add do
      begin
        CodRubr := '1';
        ideTabRubr := 'E07';
        vrRubr := 110.53;
      end;

      with itens.Add do
      begin
        CodRubr := '2';
        ideTabRubr := 'E08';
        vrRubr := 2568.89;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1210;
// var
// I : Integer;
begin
  with ACBreSocial1.Eventos.Periodicos.S1210.Add do
  begin
    EvtPgtos.Sequencial := 0;

    EvtPgtos.IdeEvento.indRetif := ireOriginal;
    // EvtPgtos.IdeEvento.NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; - obrigatório se indRetif = ireRetificacao.
    EvtPgtos.IdeEvento.IndApuracao := iapuMensal;
    EvtPgtos.IdeEvento.perApur := '052015';
    EvtPgtos.IdeEvento.TpAmb := taProducaoRestrita;
    EvtPgtos.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtPgtos.IdeEvento.VerProc := '1.0';

    EvtPgtos.IdeEmpregador.TpInsc := tiCNPJ;
    EvtPgtos.IdeEmpregador.NrInsc := '9632587410123';

    EvtPgtos.ideBenef.cpfBenef := '01478523690';
    EvtPgtos.ideBenef.deps.vrDedDep := 100.50;
    with EvtPgtos.ideBenef.InfoPgto.Add do
    begin
      DtPgto := StrToDate('10/06/2015');
      tpPgto := tpPgtoFl;
      IndResBr := tpNao;
      // -OS GRUPOS ABAIXO SÃO OPCIONAIS
      // grupo detPgtoFl agora é um collection
      with detPgtoFl.Add do
      begin
        perRef := '052015';
        ideDmDev := '2';
        indPagtoTt := tpSim;
        vrLiq := 12365.43;
        nrRecArq := '132156156';
        with retPagtoTot.Add do
        begin
          CodRubr := '1';
          ideTabRubr := '0';
          qtdRubr := 1.5;
          fatorRubr := 50;
          vrUnit := 100.10;
          vrRubr := 1001.00;
          with penAlim.Add do
          begin
            cpfBenef := '12345698745';
            dtNasctoBenef := Now;
            nmBenefic := 'Beneficiário da pensão';
            vlrPensao := 556.32;
          end;
        end;
        with infoPgtoParc.Add do
        begin
          CodRubr := '2';
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
      with detPgtoBenPr.retPgtoTot.Add do
      begin
        CodRubr := '321';
        ideTabRubr := '0';
        qtdRubr := 1.5;
        fatorRubr := 50.65;
        vrUnit := 500.85;
        vrRubr := 5001.65;
      end;
      with detPgtoBenPr.infoPgtoParc.Add do
      begin
        CodRubr := '555';
        ideTabRubr := '0';
        qtdRubr := 2;
        fatorRubr := 40.11;
        vrUnit := 842.85;
        vrRubr := 774.65;
      end;
      with detPgtoFer.Add do
      begin
        CodCateg := 111;
        dtIniGoz := Now;
        qtDias := 30;
        vrLiq := 2500.32;
        with detRubrFer.Add do
        begin
          CodRubr := '888';
          ideTabRubr := '0';
          qtdRubr := 1;
          fatorRubr := 100;
          vrUnit := 144.33;
          vrRubr := 2500.32;
          with penAlim.Add do
          begin
            cpfBenef := '44455588899';
            dtNasctoBenef := Now;
            nmBenefic := 'Beneficiário de Pensão nas Férias';
            vlrPensao := 250.32;
          end;
        end;
      end;
      with detPgtoAnt.Add do
      begin
        CodCateg := 111;
        with infoPgtoAnt.Add do
        begin
          tpBcIRRF := tpCodIncIRRF(0);
          vrBcIRRF := 2500.32;
        end;
      end;

      // grupo idePgtoExt
      idePgtoExt.idePais.codPais := '116';
      idePgtoExt.idePais.indNIF := infBeneficiaNIF;
      idePgtoExt.idePais.nifBenef := 'ABCDEFGH123456789';
      idePgtoExt.endExt.DscLograd := 'Abbey Road St';
      idePgtoExt.endExt.NrLograd := '93';
      idePgtoExt.endExt.complem := 'apto 11';
      idePgtoExt.endExt.Bairro := 'Sgt Peppers';
      idePgtoExt.endExt.NmCid := 'Liverpool';
      idePgtoExt.endExt.CodPostal := '9999999999';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1250;
begin
  with ACBreSocial1.Eventos.Periodicos.S1250.Add do
  begin
    EvtAqProd.Sequencial := 0;

    EvtAqProd.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtAqProd.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAqProd.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtAqProd.IdeEvento.perApur := '2015-06';
    EvtAqProd.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAqProd.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtAqProd.IdeEvento.VerProc := '1.0';

    EvtAqProd.IdeEmpregador.TpInsc := tiCPF;
    EvtAqProd.IdeEmpregador.NrInsc := '0123456789';

    EvtAqProd.InfoAquisProd.IdeEstabAdquir.tpInscAdq := tiCNPJ;
    EvtAqProd.InfoAquisProd.IdeEstabAdquir.nrInscAdq := '12345678910001';

    with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Add do
    begin
      indAquis := tpIdAquis(0);
      vlrTotAquis := 520000.80;

      with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
        .IdeProdutor.Add do
      begin
        tpInscProd := tiCNPJ;
        nrInscProd := '98765432100015';
        vlrBruto := 4000.54;
        vrCPDescPR := 3850.32;
        vrRatDescPR := 500.30;
        vrSenarDesc := 2500.30;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[0].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '64896549898789';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[0].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[0].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[0].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[0].InfoProcJud.Add do
        begin
          nrProcJud := '56464897';
          codSusp := 333;
          vrCPNRet := 99999.99;
          vrRatNRet := 88888.88;
          vrSenarNRet := 77777.77;
        end;
      end;

      with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
        .IdeProdutor.Add do
      begin
        tpInscProd := tiCNPJ;
        nrInscProd := '98765432100015';
        vlrBruto := 4000.54;
        vrCPDescPR := 3850.32;
        vrRatDescPR := 500.30;
        vrSenarDesc := 2500.30;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[1].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[1].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[1].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[1].Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;

        with EvtAqProd.InfoAquisProd.IdeEstabAdquir.TpAquis.Items[0]
          .IdeProdutor.Items[0].InfoProcJud.Add do
        begin
          nrProcJud := '56464897';
          codSusp := 222;
          vrCPNRet := 99999.99;
          vrRatNRet := 88888.88;
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
    EvtComProd.Sequencial := 0;

    EvtComProd.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtComProd.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtComProd.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtComProd.IdeEvento.perApur := '2015-06';
    EvtComProd.IdeEvento.TpAmb := taProducaoRestrita;
    EvtComProd.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtComProd.IdeEvento.VerProc := '1.0';

    EvtComProd.IdeEmpregador.TpInsc := tiCPF;
    EvtComProd.IdeEmpregador.NrInsc := '0123456789';

    EvtComProd.InfoComProd.IdeEstabel.nrInscEstabRural := '123456789';

    with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Add do
    begin
      indComerc := tpIndComerc(0);
      vrTotCom := 5000.80;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
        with Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
        with Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
          vrRatDescPR := 500.30;
          vrSenarDesc := 2500.30;
        end;
      end;
    end;

    with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Add do
    begin
      indComerc := tpIndComerc(1);
      vrTotCom := 5000.80;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
      end;

      with EvtComProd.InfoComProd.IdeEstabel.TpComerc.Items[1].IdeAdquir.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '99999999999999';
        vrComerc := 8888.88;
        vrRetPR := 9999.99;
        with Nfs.Add do
        begin
          serie := '00004';
          nrDocto := '648965498987894';
          dtEmisNF := Now;
          vlrBruto := 4000.54;
          vrCPDescPR := 3850.32;
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
        indComerc :=
          icComProdPorProdRuralPFInclusiveSegEspEfetuadaDirVarejoConsFinal;
        vrTotCom := 123456.65;
        with IdeAdquir.Add do
        begin
          TpInsc := tiCNPJ;
          NrInsc := '12345678901';
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
    EvtContratAvNP.Sequencial := 0;

    EvtContratAvNP.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtContratAvNP.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtContratAvNP.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtContratAvNP.IdeEvento.perApur := '2015-06';
    EvtContratAvNP.IdeEvento.TpAmb := taProducaoRestrita;
    EvtContratAvNP.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtContratAvNP.IdeEvento.VerProc := '1.0';

    EvtContratAvNP.IdeEmpregador.TpInsc := tiCNPJ;
    EvtContratAvNP.IdeEmpregador.NrInsc := '0123456789';

    with EvtContratAvNP.RemunAvNP.Add do
    begin
      TpInsc := tiCNPJ;
      NrInsc := '98765432100015';
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
      TpInsc := tpTpInsc(1);
      NrInsc := '65432198700015';
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
      TpInsc := tpTpInsc(2);
      NrInsc := '98765432100015';
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
      TpInsc := tpTpInsc(1);
      NrInsc := '11111111111111';
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
    EvtInfoComplPer.Sequencial := 0;

    EvtInfoComplPer.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtInfoComplPer.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtInfoComplPer.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtInfoComplPer.IdeEvento.perApur := '2015-06';
    EvtInfoComplPer.IdeEvento.TpAmb := taProducaoRestrita;
    EvtInfoComplPer.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtInfoComplPer.IdeEvento.VerProc := '1.0';

    EvtInfoComplPer.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtInfoComplPer.IdeEmpregador.NrInsc := '0123456789';

    EvtInfoComplPer.InfoSubstPatr.indSubstPatr := tpIndSubstPatrOpPort(0);
    EvtInfoComplPer.InfoSubstPatr.percRedContrib := 500.20;

    with EvtInfoComplPer.InfoSubstPatrOpPort.Add do
    begin
      cnpjOpPortuario := '12345678900112';
    end;

    with EvtInfoComplPer.InfoSubstPatrOpPort.Add do
    begin
      cnpjOpPortuario := '98765432100014';
    end;

    EvtInfoComplPer.InfoAtivConcom.fatorMes := 9.00;
    EvtInfoComplPer.InfoAtivConcom.fator13 := 1.00;
  end;
end;

procedure TFExemploEsocial.GerareSocial1295;
begin
  with ACBreSocial1.Eventos.Periodicos.S1295.Add do
  begin

    evtTotConting.Sequencial := 0;

    evtTotConting.IdeEvento.indRetif := tpIndRetificacao(0);
    evtTotConting.IdeEvento.NrRecibo := '65.5454.987798798798';
    evtTotConting.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    evtTotConting.IdeEvento.perApur := '2015-06';
    evtTotConting.IdeEvento.TpAmb := taProducaoRestrita;
    evtTotConting.IdeEvento.ProcEmi := TpProcEmi(0);
    evtTotConting.IdeEvento.VerProc := '1.0';

    evtTotConting.IdeEmpregador.TpInsc := tpTpInsc(1);
    evtTotConting.IdeEmpregador.NrInsc := '0123456789';

    evtTotConting.IdeRespInf.nmResp := 'Responsavel teste';
    evtTotConting.IdeRespInf.cpfResp := '12345678950';
    evtTotConting.IdeRespInf.Telefone := '46 - 22222222';
    evtTotConting.IdeRespInf.email := 'Responsavelteste@email.com';

  end;
end;

procedure TFExemploEsocial.GerareSocial1298;
begin
  with ACBreSocial1.Eventos.Periodicos.S1298.Add do
  begin
    EvtReabreEvPer.Sequencial := 0;

    EvtReabreEvPer.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtReabreEvPer.IdeEvento.perApur := '2015-06';
    EvtReabreEvPer.IdeEvento.TpAmb := taProducaoRestrita;
    EvtReabreEvPer.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtReabreEvPer.IdeEvento.VerProc := '1.0';

    EvtReabreEvPer.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtReabreEvPer.IdeEmpregador.NrInsc := '0123456789';
  end;
end;

procedure TFExemploEsocial.GerareSocial1299;
begin
  with ACBreSocial1.Eventos.Periodicos.S1299.Add do
  begin
    EvtFechaEvPer.Sequencial := 0;

    EvtFechaEvPer.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtFechaEvPer.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtFechaEvPer.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtFechaEvPer.IdeEvento.perApur := '2015-06';
    EvtFechaEvPer.IdeEvento.TpAmb := taProducaoRestrita;
    EvtFechaEvPer.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtFechaEvPer.IdeEvento.VerProc := '1.0';

    EvtFechaEvPer.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtFechaEvPer.IdeEmpregador.NrInsc := '0123456789';

    EvtFechaEvPer.IdeRespInf.nmResp := 'Responsavel teste';
    EvtFechaEvPer.IdeRespInf.cpfResp := '12345678950';
    EvtFechaEvPer.IdeRespInf.Telefone := '46 - 22222222';
    EvtFechaEvPer.IdeRespInf.email := 'Responsavelteste@email.com';

    EvtFechaEvPer.InfoFech.evtRemun := tpSimNao(0);
    EvtFechaEvPer.InfoFech.EvtPgtos := tpSimNao(1);
    EvtFechaEvPer.InfoFech.EvtAqProd := tpSimNao(1);
    EvtFechaEvPer.InfoFech.EvtComProd := tpSimNao(0);
    EvtFechaEvPer.InfoFech.EvtContratAvNP := tpSimNao(1);
    EvtFechaEvPer.InfoFech.EvtInfoComplPer := tpSimNao(0);
    EvtFechaEvPer.InfoFech.compSemMovto := '07-2015';
  end;
end;

procedure TFExemploEsocial.GerareSocial1300;
begin
  with ACBreSocial1.Eventos.Periodicos.S1300.Add do
  begin
    EvtContrSindPatr.Sequencial := 0;

    EvtContrSindPatr.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtContrSindPatr.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtContrSindPatr.IdeEvento.IndApuracao := tpIndApuracao(iapuMensal);
    EvtContrSindPatr.IdeEvento.perApur := '2015-06';
    EvtContrSindPatr.IdeEvento.TpAmb := taProducaoRestrita;
    EvtContrSindPatr.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtContrSindPatr.IdeEvento.VerProc := '1.0';

    EvtContrSindPatr.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtContrSindPatr.IdeEmpregador.NrInsc := '0123456789';

    with EvtContrSindPatr.ContribSind.Add do
    begin
      cnpjSindic := '01234567891111';
      tpContribSind := tpTpContribSind(0);
      vlrContribSind := 1500.50;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2190;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2190.Add do
  begin
    EvtAdmPrelim.Sequencial := 0;

    EvtAdmPrelim.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAdmPrelim.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtAdmPrelim.IdeEvento.VerProc := '1.0';

    EvtAdmPrelim.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtAdmPrelim.IdeEmpregador.NrInsc := '12345678901234';

    EvtAdmPrelim.InfoRegPrelim.CpfTrab := '12345678901';
    EvtAdmPrelim.InfoRegPrelim.DtNascto := Now - 9125;
    EvtAdmPrelim.InfoRegPrelim.DtAdm := Now;
  end;
end;

procedure TFExemploEsocial.GerareSocial2200;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2200.Add do
  begin
    EvtAdmissao.Sequencial := 0;

    // EvtAdmissao.IdeEvento.indRetif := tpIndRetificacao(1);
    EvtAdmissao.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAdmissao.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAdmissao.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtAdmissao.IdeEvento.VerProc := '1.0';

    EvtAdmissao.IdeEmpregador.TpInsc := tiCNPJ;
    EvtAdmissao.IdeEmpregador.NrInsc := '12345678901234';

    EvtAdmissao.Trabalhador.CpfTrab := '54564654564';
    EvtAdmissao.Trabalhador.NisTrab := '12345678901';
    EvtAdmissao.Trabalhador.NmTrab := 'Empregado teste';
    EvtAdmissao.Trabalhador.Sexo := 'M';
    EvtAdmissao.Trabalhador.RacaCor := 1;
    EvtAdmissao.Trabalhador.EstCiv := 1;
    EvtAdmissao.Trabalhador.GrauInstr := '10';
    EvtAdmissao.Trabalhador.IndPriEmpr := tpNao;
    EvtAdmissao.Trabalhador.nmSoc := 'Nome social';

    EvtAdmissao.Trabalhador.Nascimento.DtNascto := date;
    EvtAdmissao.Trabalhador.Nascimento.codMunic := 51268;
    EvtAdmissao.Trabalhador.Nascimento.uf := 'PR';
    EvtAdmissao.Trabalhador.Nascimento.PaisNascto := '565';
    EvtAdmissao.Trabalhador.Nascimento.PaisNac := '545';
    EvtAdmissao.Trabalhador.Nascimento.NmMae := 'teste mae';
    EvtAdmissao.Trabalhador.Nascimento.NmPai := 'teste pai';

    EvtAdmissao.Trabalhador.Documentos.CTPS.NrCtps := '56454';
    EvtAdmissao.Trabalhador.Documentos.CTPS.SerieCtps := '646';
    EvtAdmissao.Trabalhador.Documentos.CTPS.UfCtps := 'PR';

    EvtAdmissao.Trabalhador.Documentos.RIC.NrRic := '123123';
    EvtAdmissao.Trabalhador.Documentos.RIC.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.RIC.DtExped := date;

    EvtAdmissao.Trabalhador.Documentos.RG.NrRg := '123123';
    EvtAdmissao.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.RG.DtExped := date;

    EvtAdmissao.Trabalhador.Documentos.RNE.NrRne := '123123';
    EvtAdmissao.Trabalhador.Documentos.RNE.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.RNE.DtExped := date;

    EvtAdmissao.Trabalhador.Documentos.OC.NrOc := '999';
    EvtAdmissao.Trabalhador.Documentos.OC.OrgaoEmissor := 'SSP';
    EvtAdmissao.Trabalhador.Documentos.OC.DtExped := date;
    EvtAdmissao.Trabalhador.Documentos.OC.DtValid := date;

    EvtAdmissao.Trabalhador.Documentos.CNH.nrRegCnh := '999';
    EvtAdmissao.Trabalhador.Documentos.CNH.DtExped := date;
    EvtAdmissao.Trabalhador.Documentos.CNH.ufCnh := tpuf(ufPR);
    EvtAdmissao.Trabalhador.Documentos.CNH.DtValid := date;
    EvtAdmissao.Trabalhador.Documentos.CNH.dtPriHab := date;
    EvtAdmissao.Trabalhador.Documentos.CNH.categoriaCnh := tpCnh(cnA);

    EvtAdmissao.Trabalhador.Endereco.Brasil.TpLograd := 'RUA';
    EvtAdmissao.Trabalhador.Endereco.Brasil.DscLograd := 'TESTE';
    EvtAdmissao.Trabalhador.Endereco.Brasil.NrLograd := '777';
    EvtAdmissao.Trabalhador.Endereco.Brasil.Complemento := 'AP 101';
    EvtAdmissao.Trabalhador.Endereco.Brasil.Bairro := 'CENTRO';
    EvtAdmissao.Trabalhador.Endereco.Brasil.Cep := '85500000';
    EvtAdmissao.Trabalhador.Endereco.Brasil.codMunic := 11111;
    EvtAdmissao.Trabalhador.Endereco.Brasil.uf := tpuf(ufPR);

    EvtAdmissao.Trabalhador.Endereco.Exterior.PaisResid := '545';
    EvtAdmissao.Trabalhador.Endereco.Exterior.DscLograd := 'TESTE';
    EvtAdmissao.Trabalhador.Endereco.Exterior.NrLograd := '777';
    EvtAdmissao.Trabalhador.Endereco.Exterior.Complemento := 'AP 101';
    EvtAdmissao.Trabalhador.Endereco.Exterior.Bairro := 'CENTRO';
    EvtAdmissao.Trabalhador.Endereco.Exterior.NmCid := 'CIDADE EXTERIOR';
    EvtAdmissao.Trabalhador.Endereco.Exterior.CodPostal := '50000';

    EvtAdmissao.Trabalhador.TrabEstrangeiro.DtChegada := date;
    EvtAdmissao.Trabalhador.TrabEstrangeiro.ClassTrabEstrang :=
      tpClassTrabEstrang(ctVistoPermanente);
    EvtAdmissao.Trabalhador.TrabEstrangeiro.CasadoBr := 'N';
    EvtAdmissao.Trabalhador.TrabEstrangeiro.FilhosBr := 'N';

    EvtAdmissao.Trabalhador.InfoDeficiencia.DefFisica := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefVisual := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefAuditiva := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefMental := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.ReabReadap := tpSim;
    EvtAdmissao.Trabalhador.InfoDeficiencia.infoCota := tpNao;
    EvtAdmissao.Trabalhador.InfoDeficiencia.observacao := 'sem deficiencia';

    with EvtAdmissao.Trabalhador.Dependente.Add do
    begin
      tpDep := tdConjuge;
      nmDep := 'Dependente 1';
      DtNascto := date;
      cpfDep := '12345678901';
      depIRRF := tpSim;
      depSF := tpNao;
      incTrab := tpNao;
    end;

    with EvtAdmissao.Trabalhador.Dependente.Add do
    begin
      tpDep := tdFilhoOuEnteado;
      nmDep := 'Dependente 2';
      DtNascto := date;
      cpfDep := '12345678901';
      depIRRF := tpSim;
      depSF := tpNao;
      incTrab := tpNao;
    end;

    EvtAdmissao.Trabalhador.Aposentadoria.TrabAposent := tpNao;

    EvtAdmissao.Trabalhador.Contato.FonePrinc := '91067240';
    EvtAdmissao.Trabalhador.Contato.FoneAlternat := '91067240';
    EvtAdmissao.Trabalhador.Contato.EmailPrinc := 'TESTE@email.com.br';
    EvtAdmissao.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';

    EvtAdmissao.Vinculo.Matricula := '54545';
    EvtAdmissao.Vinculo.TpRegTrab := tpTpRegTrab(1);
    EvtAdmissao.Vinculo.TpRegPrev := tpTpRegPrev(1);
    EvtAdmissao.Vinculo.NrRecInfPrelim := '9999999999';

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.DtAdm := date;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.TpAdmissao :=
      tpTpAdmissao(1);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.IndAdmissao :=
      tpTpIndAdmissao(iaNormal);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.TpRegJor := tpTpRegJor(1);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.NatAtividade :=
      tpNatAtividade(navUrbano);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.dtBase := 03;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf :=
      '12345678901234';

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.FGTS.OpcFGTS :=
      tpOpcFGTS(ofOptante);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.FGTS.DtOpcFGTS := date;

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.hipLeg := 1;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr
      := 'teste';
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.tpinclContr
      := tpinclContr(0);

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.
      IdeTomadorServ.TpInsc := tiCNPJ;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.
      IdeTomadorServ.NrInsc := '12345678901234';
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.
      IdeTomadorServ.ideEstabVinc.TpInsc := tiCNPJ;
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.
      IdeTomadorServ.ideEstabVinc.NrInsc := '12345678901234';

    with EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.
      IdeTrabSubstituido.Add do
      CpfTrabSubst := '12345678912';

    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.aprend.TpInsc :=
      tpTpInsc(1);
    EvtAdmissao.Vinculo.InfoRegimeTrab.InfoCeletista.aprend.NrInsc :=
      '98765432109';

    // enviar apenas um tipo de admissao
    // EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.IndProvim   := tpIndProvim(ipNormal);
    // EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.TpProv      := tpTpProv(tpNomeacaoCargoEfetivo);
    // EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.DtNomeacao  := Date;
    // EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.DtPosse     := Date;
    // EvtAdmissao.Vinculo.InfoRegimeTrab.InfoEstatutario.DtExercicio := Date;
    // end;

    EvtAdmissao.Vinculo.InfoContrato.CodCargo := '545';
    EvtAdmissao.Vinculo.InfoContrato.CodFuncao := '5456';
    EvtAdmissao.Vinculo.InfoContrato.CodCateg := 111;
    EvtAdmissao.Vinculo.InfoContrato.codCarreira := '1';
    EvtAdmissao.Vinculo.InfoContrato.dtIngrCarr := Now;

    EvtAdmissao.Vinculo.InfoContrato.Remuneracao.VrSalFx := 5000;
    EvtAdmissao.Vinculo.InfoContrato.Remuneracao.UndSalFixo := tpUndSalFixo(5);
    EvtAdmissao.Vinculo.InfoContrato.Remuneracao.DscSalVar := 'nada a declarar';

    EvtAdmissao.Vinculo.InfoContrato.Duracao.TpContr := tpTpContr(1);
    EvtAdmissao.Vinculo.InfoContrato.Duracao.dtTerm := date;

    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.TpInsc
      := tiCNPJ;
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.NrInsc :=
      '21354632';
    EvtAdmissao.Vinculo.InfoContrato.LocalTrabalho.LocalTrabGeral.DescComp :=
      'Descricao local geral teste';

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
    EvtAdmissao.Vinculo.InfoContrato.HorContratual.DscTpJorn :=
      'horario contratual';
    EvtAdmissao.Vinculo.InfoContrato.HorContratual.tmpParc :=
      tpNaoeTempoParcial;

    with EvtAdmissao.Vinculo.InfoContrato.HorContratual.horario.Add do
    begin
      Dia := tpTpDia(diSegundaFeira);
      codHorContrat := '54';
    end;

    with EvtAdmissao.Vinculo.InfoContrato.HorContratual.horario.Add do
    begin
      Dia := tpTpDia(diTercaFeira);
      codHorContrat := '10';
    end;

    with EvtAdmissao.Vinculo.InfoContrato.FiliacaoSindical.Add do
      CnpjSindTrab := '12345678901234';

    EvtAdmissao.Vinculo.InfoContrato.AlvaraJudicial.nrProcJud := '123';

    with EvtAdmissao.Vinculo.InfoContrato.observacoes.Add do
      observacao := 'Observacao';

    EvtAdmissao.Vinculo.SucessaoVinc.cnpjEmpregAnt := '12345678901234';
    EvtAdmissao.Vinculo.SucessaoVinc.MatricAnt := '123';
    EvtAdmissao.Vinculo.SucessaoVinc.dtTransf := date;
    EvtAdmissao.Vinculo.SucessaoVinc.observacao := 'transferido';

    EvtAdmissao.Vinculo.transfDom.cpfSubstituido := '12345678901';
    EvtAdmissao.Vinculo.transfDom.MatricAnt := '123';
    EvtAdmissao.Vinculo.transfDom.dtTransf := date;

    EvtAdmissao.Vinculo.Afastamento.DtIniAfast := Now;
    EvtAdmissao.Vinculo.Afastamento.codMotAfast := mtvAcidenteDoencaTrabalho;

    EvtAdmissao.Vinculo.Desligamento.DtDeslig := Now;
  end;
end;

procedure TFExemploEsocial.GerareSocial2205;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2205.Add do
  begin
    EvtAltCadastral.Sequencial := 0;

    EvtAltCadastral.IdeEvento.indRetif := tpIndRetificacao(1);
    EvtAltCadastral.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAltCadastral.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAltCadastral.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtAltCadastral.IdeEvento.VerProc := '1.0';

    EvtAltCadastral.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtAltCadastral.IdeEmpregador.NrInsc := '12345678901234';

    EvtAltCadastral.ideTrabalhador.CpfTrab := '12345678901';

    EvtAltCadastral.dtAlteracao := Now;

    EvtAltCadastral.Trabalhador.NisTrab := '12345678901';
    EvtAltCadastral.Trabalhador.NmTrab := 'Empregado teste';
    EvtAltCadastral.Trabalhador.Sexo := 'M';
    EvtAltCadastral.Trabalhador.RacaCor := 1;
    EvtAltCadastral.Trabalhador.EstCiv := 1;
    EvtAltCadastral.Trabalhador.GrauInstr := '10';
    EvtAltCadastral.Trabalhador.nmSoc := 'Nome social';

    EvtAltCadastral.Trabalhador.Documentos.CTPS.NrCtps := '56454';
    EvtAltCadastral.Trabalhador.Documentos.CTPS.SerieCtps := '646';
    EvtAltCadastral.Trabalhador.Documentos.CTPS.UfCtps := 'PR';

    EvtAltCadastral.Trabalhador.Documentos.RIC.NrRic := '123123';
    EvtAltCadastral.Trabalhador.Documentos.RIC.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.RIC.DtExped := date;

    EvtAltCadastral.Trabalhador.Documentos.RG.NrRg := '123123';
    EvtAltCadastral.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.RG.DtExped := date;

    EvtAltCadastral.Trabalhador.Documentos.RNE.NrRne := '123123';
    EvtAltCadastral.Trabalhador.Documentos.RNE.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.RNE.DtExped := date;

    EvtAltCadastral.Trabalhador.Documentos.OC.NrOc := '999';
    EvtAltCadastral.Trabalhador.Documentos.OC.OrgaoEmissor := 'SSP';
    EvtAltCadastral.Trabalhador.Documentos.OC.DtExped := date;
    EvtAltCadastral.Trabalhador.Documentos.OC.DtValid := date;

    EvtAltCadastral.Trabalhador.Documentos.CNH.nrRegCnh := '999';
    EvtAltCadastral.Trabalhador.Documentos.CNH.DtExped := date;
    EvtAltCadastral.Trabalhador.Documentos.CNH.ufCnh := tpuf(ufPR);
    EvtAltCadastral.Trabalhador.Documentos.CNH.DtValid := date;
    EvtAltCadastral.Trabalhador.Documentos.CNH.dtPriHab := date;
    EvtAltCadastral.Trabalhador.Documentos.CNH.categoriaCnh := tpCnh(cnA);

    EvtAltCadastral.Trabalhador.Endereco.Brasil.TpLograd := 'RUA';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.DscLograd := 'TESTE';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.NrLograd := '777';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.Complemento := 'AP 101';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.Bairro := 'CENTRO';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.Cep := '85500000';
    EvtAltCadastral.Trabalhador.Endereco.Brasil.codMunic := 11111;
    EvtAltCadastral.Trabalhador.Endereco.Brasil.uf := tpuf(ufPR);

    EvtAltCadastral.Trabalhador.Endereco.Exterior.PaisResid := '545';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.DscLograd := 'TESTE';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.NrLograd := '777';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.Complemento := 'AP 101';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.Bairro := 'CENTRO';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.NmCid := 'CIDADE EXTERIOR';
    EvtAltCadastral.Trabalhador.Endereco.Exterior.CodPostal := '50000';

    EvtAltCadastral.Trabalhador.TrabEstrangeiro.DtChegada := date;
    EvtAltCadastral.Trabalhador.TrabEstrangeiro.ClassTrabEstrang :=
      tpClassTrabEstrang(ctVistoPermanente);
    EvtAltCadastral.Trabalhador.TrabEstrangeiro.CasadoBr := 'N';
    EvtAltCadastral.Trabalhador.TrabEstrangeiro.FilhosBr := 'N';

    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefFisica := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefVisual := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefAuditiva := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefMental := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.ReabReadap := tpSim;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.infoCota := tpNao;
    EvtAltCadastral.Trabalhador.InfoDeficiencia.observacao := 'sem deficiencia';

    with EvtAltCadastral.Trabalhador.Dependente.Add do
    begin
      tpDep := tdConjuge;
      nmDep := 'Dependente 1';
      DtNascto := date;
      cpfDep := '12345678901';
      depIRRF := tpSim;
      depSF := tpNao;
      incTrab := tpNao;
    end;

    with EvtAltCadastral.Trabalhador.Dependente.Add do
    begin
      tpDep := tdFilhoOuEnteado;
      nmDep := 'Dependente 2';
      DtNascto := date;
      cpfDep := '12345678901';
      depIRRF := tpSim;
      depSF := tpNao;
      incTrab := tpNao
    end;

    EvtAltCadastral.Trabalhador.Aposentadoria.TrabAposent := tpNao;

    EvtAltCadastral.Trabalhador.Contato.FonePrinc := '91067240';
    EvtAltCadastral.Trabalhador.Contato.FoneAlternat := '91067240';
    EvtAltCadastral.Trabalhador.Contato.EmailPrinc := 'TESTE@email.com.br';
    EvtAltCadastral.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';
  end;
end;

procedure TFExemploEsocial.GerareSocial2206;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2206.Add do
  begin
    EvtAltContratual.Sequencial := 0;

    EvtAltContratual.IdeEvento.indRetif := ireOriginal;
    // EvtAltContratual.IdeEvento.NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif = ireRetificacao;
    EvtAltContratual.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAltContratual.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtAltContratual.IdeEvento.VerProc := '1.0';

    EvtAltContratual.IdeEmpregador.TpInsc := tiCNPJ;
    EvtAltContratual.IdeEmpregador.NrInsc := '12345678901234';

    EvtAltContratual.IdeVinculo.CpfTrab := '12345678901';
    EvtAltContratual.IdeVinculo.NisTrab := '96325874103';
    EvtAltContratual.IdeVinculo.Matricula := 'A1234';

    EvtAltContratual.AltContratual.dtAlteracao := date;
    EvtAltContratual.AltContratual.dtEf := Now;
    EvtAltContratual.AltContratual.dscAlt := 'descrição da alteração';

    EvtAltContratual.AltContratual.Vinculo.TpRegTrab := trCLT;
    EvtAltContratual.AltContratual.Vinculo.TpRegPrev := rpRGPS;

    EvtAltContratual.AltContratual.InfoRegimeTrab.InfoCeletista.TpRegJor :=
      rjSubmetidosHorarioTrabalho;
    EvtAltContratual.AltContratual.InfoRegimeTrab.InfoCeletista.NatAtividade :=
      navUrbano;
    EvtAltContratual.AltContratual.InfoRegimeTrab.InfoCeletista.dtBase := 08;
    EvtAltContratual.AltContratual.InfoRegimeTrab.InfoCeletista.
      cnpjSindCategProf := '15975395135700';

    EvtAltContratual.AltContratual.InfoRegimeTrab.InfoCeletista.trabTemporario.
      justProrr := 'Prorrogado porque eu quis';

    // EvtAltContratual.AltContratual.infoRegimeTrab.InfoEstatutario.tpPlanRP := tpPlanRP(0);

    EvtAltContratual.AltContratual.InfoContrato.CodCargo := '123';
    EvtAltContratual.AltContratual.InfoContrato.CodFuncao := '321';
    EvtAltContratual.AltContratual.InfoContrato.CodCateg := 111;
    EvtAltContratual.AltContratual.InfoContrato.codCarreira := '1';
    EvtAltContratual.AltContratual.InfoContrato.dtIngrCarr := Now;

    EvtAltContratual.AltContratual.InfoContrato.Remuneracao.VrSalFx := 780.00;
    EvtAltContratual.AltContratual.InfoContrato.Remuneracao.UndSalFixo
      := sfPorMes;
    EvtAltContratual.AltContratual.InfoContrato.Remuneracao.DscSalVar :=
      'Descrição de salário variável, obrigatório caso UndSalFixo for sfNaoAplicavel';

    EvtAltContratual.AltContratual.InfoContrato.Duracao.TpContr :=
      PrazoIndeterminado;
    // EvtAltContratual.AltContratual.infoContrato.Duracao.dtTerm  := Date; Obrigatório se TpContr = PrazoDeterminado!

    // LocalTrabGeral não deve ser preenchido no caso de trabalhador doméstico.
    EvtAltContratual.AltContratual.InfoContrato.LocalTrabalho.LocalTrabGeral.
      TpInsc := tiCNPJ;
    EvtAltContratual.AltContratual.InfoContrato.LocalTrabalho.LocalTrabGeral.
      NrInsc := '12345678901234';
    EvtAltContratual.AltContratual.InfoContrato.LocalTrabalho.LocalTrabGeral.
      DescComp := 'Descrição complementar do local de trabalho.';

    // LocalTrabDom - exclusivo para trabalhador doméstico, indicando endereço onde exerce suas atividades
    (* EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.TpLograd    := '001';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.DscLograd   := 'Rua das Hortencias';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.NrLograd    := '12';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Complemento := 'Fundos';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Bairro      := 'Jardim das Flores';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Cep         := '11001001';
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.CodMunic    := 1234567;
      EvtAltContratual.AltContratual.infoContrato.LocalTrabalho.LocalTrabDom.Uf          := ufPr; *)

    EvtAltContratual.AltContratual.InfoContrato.HorContratual.QtdHrsSem := 44;
    EvtAltContratual.AltContratual.InfoContrato.HorContratual.TpJornada :=
      tjDemaisTiposJornada;
    EvtAltContratual.AltContratual.InfoContrato.HorContratual.DscTpJorn :=
      'Descrição do tipo de jornada, obrigatório se tpJornada = tjDemaisTiposJornada';
    EvtAltContratual.AltContratual.InfoContrato.HorContratual.tmpParc :=
      tpNaoeTempoParcial;

    with EvtAltContratual.AltContratual.InfoContrato.HorContratual.
      horario.Add do
    begin
      Dia := diSegundaFeira;
      codHorContrat := '001';
    end;

    with EvtAltContratual.AltContratual.InfoContrato.FiliacaoSindical.Add do
      CnpjSindTrab := '12345678901234';

    EvtAltContratual.AltContratual.InfoContrato.AlvaraJudicial.nrProcJud
      := '543216';
    EvtAltContratual.AltContratual.InfoContrato.servPubl.mtvAlter := maPromocao;

  end;
end;

procedure TFExemploEsocial.GerareSocial2210;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2210.Add do
  begin
    EvtCAT.Sequencial := 0;

    EvtCAT.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtCAT.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtCAT.IdeEvento.TpAmb := taProducaoRestrita;
    EvtCAT.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtCAT.IdeEvento.VerProc := '1.0';

    EvtCAT.IdeRegistrador.tpRegistrador := tpTpRegistrador(0);
    EvtCAT.IdeRegistrador.TpInsc := tpTpInsc(1);
    EvtCAT.IdeRegistrador.NrInsc := '12345678901234';

    EvtCAT.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtCAT.IdeEmpregador.NrInsc := '12345678901234';

    EvtCAT.ideTrabalhador.CpfTrab := '12345678901';
    EvtCAT.ideTrabalhador.NisTrab := '12345678901';

    EvtCAT.Cat.dtAcid := Now;
    EvtCAT.Cat.TpAcid := '1.0.01';
    EvtCAT.Cat.hrAcid := '1200';

    EvtCAT.Cat.hrsTrabAntesAcid := '0400';
    EvtCAT.Cat.tpCat := tpTpCat(0);
    EvtCAT.Cat.indCatObito := tpNao;
    EvtCAT.Cat.dtOBito := Now;
    EvtCAT.Cat.indComunPolicia := tpSim;
    EvtCAT.Cat.codSitGeradora := 200004300;
    EvtCAT.Cat.iniciatCAT := tpIniciatCAT(1);
    EvtCAT.Cat.observacao := 'Teste';

    EvtCAT.Cat.LocalAcidente.tpLocal := tpTpLocal(1);
    EvtCAT.Cat.LocalAcidente.dscLocal := 'Local Teste';
    EvtCAT.Cat.LocalAcidente.DscLograd := 'Logradouro Teste';
    EvtCAT.Cat.LocalAcidente.NrLograd := '111';
    EvtCAT.Cat.LocalAcidente.codMunic := 123;
    EvtCAT.Cat.LocalAcidente.uf := tpuf(ufPR);
    EvtCAT.Cat.LocalAcidente.cnpjLocalAcid := '12345678901234';
    EvtCAT.Cat.LocalAcidente.pais := '008';
    EvtCAT.Cat.LocalAcidente.CodPostal := '6546';

    with EvtCAT.Cat.ParteAtingida.Add do
    begin
      codParteAting := 753030000;
      lateralidade := tpLateralidade(1);
    end;

    with EvtCAT.Cat.ParteAtingida.Add do
    begin
      codParteAting := 753070700;
      lateralidade := tpLateralidade(2);
    end;

    with EvtCAT.Cat.ParteAtingida.Add do
    begin
      codParteAting := 753510200;
      lateralidade := tpLateralidade(3);
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

    EvtCAT.Cat.Atestado.codCNES := '1234567';
    EvtCAT.Cat.Atestado.dtAtendimento := Now;
    EvtCAT.Cat.Atestado.hrAtendimento := '1330';
    EvtCAT.Cat.Atestado.indInternacao := tpSim;
    EvtCAT.Cat.Atestado.durTrat := 5;
    EvtCAT.Cat.Atestado.indAfast := tpSim;
    EvtCAT.Cat.Atestado.dscLesao := 1;
    EvtCAT.Cat.Atestado.dscCompLesao := 'Descricao complementar';
    EvtCAT.Cat.Atestado.diagProvavel := 'Diagnostico teste';
    EvtCAT.Cat.Atestado.codCID := '1234';
    EvtCAT.Cat.Atestado.observacao := 'Observação teste';

    EvtCAT.Cat.Atestado.Emitente.nmEmit := 'Emitente Teste';
    EvtCAT.Cat.Atestado.Emitente.ideOC := tpIdeOC(1);
    EvtCAT.Cat.Atestado.Emitente.NrOc := '456123';
    EvtCAT.Cat.Atestado.Emitente.ufOC := tpuf(ufPR);

    EvtCAT.Cat.CatOrigem.dtCatOrig := Now;
    EvtCAT.Cat.CatOrigem.nrCatOrig := '123456';
  end;
end;

procedure TFExemploEsocial.GerareSocial2220;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2220.Add do
  begin
    EvtASO.Sequencial := 0;

    EvtASO.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtASO.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtASO.IdeEvento.TpAmb := TpTpAmb(1);
    EvtASO.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtASO.IdeEvento.VerProc := '1.0';

    EvtASO.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtASO.IdeEmpregador.NrInsc := '12345678901234';

    EvtASO.IdeVinculo.CpfTrab := '12345678901';
    EvtASO.IdeVinculo.NisTrab := '12345678901';
    EvtASO.IdeVinculo.Matricula := '5000';

    EvtASO.Aso.DtAso := date;
    EvtASO.Aso.tpAso := tpTpAso(1);
    EvtASO.Aso.ResAso := tpResAso(1);

    with EvtASO.Aso.Exame.Add do
    begin
      DtExm := date;
      procRealizado := 123;
      obsProc := 'observação do procedimento realizado';
      interprExm := tpInterprExm(0);
      ordExame := tpOrdExame(0);
      dtIniMonit := Now;
      dtFimMonit := Now;
      indResult := tpIndResult(1);

      RespMonit.NisResp := '12345678901';
      RespMonit.NrConsClasse := '7893';
      RespMonit.UfConsClasse := tpuf(ufPR);
    end;

    with EvtASO.Aso.Exame.Add do
    begin
      DtExm := date + 1;
      procRealizado := 456;
      obsProc := 'observação do procedimento realizado';
      ordExame := tpOrdExame(0);
      dtIniMonit := Now;
      dtFimMonit := Now;
      indResult := tpIndResult(1);

      RespMonit.NisResp := '12345678901';
      RespMonit.NrConsClasse := '7893';
      RespMonit.UfConsClasse := tpuf(ufPR);
    end;

    EvtASO.Aso.IdeServSaude.codCNES := '9876541';
    EvtASO.Aso.IdeServSaude.FrmCtt := 'Telefone: 32200000';
    EvtASO.Aso.IdeServSaude.email := 'teste@teste.com';
    EvtASO.Aso.IdeServSaude.Medico.NmMed := 'MEDICO TESTE';
    EvtASO.Aso.IdeServSaude.Medico.CRM.NrCRM := '88888888';
    EvtASO.Aso.IdeServSaude.Medico.CRM.UfCRM := tpuf(ufPR);
  end;
end;

procedure TFExemploEsocial.GerareSocial2230;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2230.Add do
  begin
    EvtAfastTemp.Sequencial := 0;

    EvtAfastTemp.IdeEvento.indRetif := ireOriginal;
    // EvtAfastTemp.IdeEvento.NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif=ireRetificacao
    EvtAfastTemp.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAfastTemp.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtAfastTemp.IdeEvento.VerProc := '1.0';

    EvtAfastTemp.IdeEmpregador.TpInsc := tiCNPJ;
    EvtAfastTemp.IdeEmpregador.NrInsc := '12345678901234';

    EvtAfastTemp.IdeVinculo.CpfTrab := '12345678901';
    EvtAfastTemp.IdeVinculo.NisTrab := '12345678901';
    EvtAfastTemp.IdeVinculo.Matricula := 'A123';

    EvtAfastTemp.infoAfastamento.iniAfastamento.DtIniAfast := Now;
    EvtAfastTemp.infoAfastamento.iniAfastamento.codMotAfast := mtvAcidenteDoencaTrabalho;
    EvtAfastTemp.infoAfastamento.iniAfastamento.infoMesmoMtv := tpNao;
    EvtAfastTemp.infoAfastamento.iniAfastamento.tpAcidTransito := tpatOutros;
    EvtAfastTemp.infoAfastamento.iniAfastamento.observacao :=
      'Campo opcional, salvo quando codMotAfast=21 aí é obrigatória';

    with EvtAfastTemp.infoAfastamento.iniAfastamento.infoAtestado.Add do
    begin
      codCID := '1234';
      qtDiasAfast := 13;

      Emitente.nmEmit := 'João das Neves';
      Emitente.ideOC := idCRM;
      Emitente.NrOc := '3690';
      Emitente.ufOC := ufPR;
    end;

    // infoCessao opcional usado para afastamento por cessão de funcionário. Ex.: Orgão Público, Sindicatos, etc...
    EvtAfastTemp.infoAfastamento.iniAfastamento.infoCessao.cnpjCess :=
      '78945612303216';
    EvtAfastTemp.infoAfastamento.iniAfastamento.infoCessao.infOnus :=
      ocCessionario;

    // infoMandSind opcional para cessão de funcionario para mandato sindical
    EvtAfastTemp.infoAfastamento.iniAfastamento.infoMandSind.cnpjSind :=
      '12345678901234';
    EvtAfastTemp.infoAfastamento.iniAfastamento.infoMandSind.infOnusRemun :=
      orEmpregador;

    // Apenas alteração do MOTIVO de afastamento
    EvtAfastTemp.infoAfastamento.altAfastamento.dtAltMot := date;
    EvtAfastTemp.infoAfastamento.altAfastamento.codMotAnt := '16';
    EvtAfastTemp.infoAfastamento.altAfastamento.codMotAfast := '15';
    EvtAfastTemp.infoAfastamento.altAfastamento.infoMesmoMtv := tpNao;
    EvtAfastTemp.infoAfastamento.altAfastamento.indEfRetroativo := tpSim;
    EvtAfastTemp.infoAfastamento.altAfastamento.origAlt := tpOrigemAltAfast(0);
    EvtAfastTemp.infoAfastamento.altAfastamento.nrProcJud := '231321321';

    EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.codCID := '0123';
    EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.qtdDiasAfast := 10;
    EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.nmEmit :=
      'Nome do emitente na alteração';
    EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.ideOC := tpIdeOC(0);
    EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.NrOc := '12313';
    EvtAfastTemp.infoAfastamento.altAfastamento.altEmpr.ufOC := ufSP;

    // informações de término do Afastamento
    EvtAfastTemp.infoAfastamento.fimAfastamento.dtTermAfast := Now;
    // EvtAfastTemp.infoAfastamento.fimAfastamento.codMotAfast := '15';
    // EvtAfastTemp.infoAfastamento.fimAfastamento.infoMesmoMtv := tpNao;
  end;
end;

procedure TFExemploEsocial.GerareSocial2240;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2240.Add do
  begin
    EvtExpRisco.Sequencial := 0;

    EvtExpRisco.IdeEvento.indRetif := ireOriginal;
    EvtExpRisco.IdeEvento.NrRecibo := '654654865656';
    EvtExpRisco.IdeEvento.TpAmb := taProducaoRestrita;
    EvtExpRisco.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtExpRisco.IdeEvento.VerProc := '1.0';

    EvtExpRisco.IdeEmpregador.TpInsc := tiCNPJ;
    EvtExpRisco.IdeEmpregador.NrInsc := '12345678901234';

    EvtExpRisco.IdeVinculo.CpfTrab := '12345678901';
    EvtExpRisco.IdeVinculo.NisTrab := '12345678901';
    EvtExpRisco.IdeVinculo.Matricula := '564545';

    EvtExpRisco.infoExpRisco.iniExpRisco.dtCondicao := date;

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

        with epcEpi.epc.Add do
        begin
          dscEpc := 'Descrição do EPC 1';
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

    // alteração das informações de condições de ambiente de trabalho, opcional
    EvtExpRisco.infoExpRisco.altExpRisco.dtCondicao := date;

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

        with epcEpi.epc.Add do
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

    // fimExpRisco - opcional, informar quando o trabalhador não se sujeitar mais as condições de ambiente informadas anteriormente
    EvtExpRisco.infoExpRisco.fimExpRisco.dtFimCondicao := date;

    with EvtExpRisco.infoExpRisco.fimExpRisco.InfoAmb.Add do
    begin
      codAmb := '897654987';
    end;

    with EvtExpRisco.infoExpRisco.respReg.Add do
    begin
      dtIni := Now;
      dtFim := Now;
      NisResp := '12345678901';
      NrOc := '51561561';
      ufOC := ufSP;
    end;

    with EvtExpRisco.infoExpRisco.respReg.Add do
    begin
      dtIni := Now;
      dtFim := Now;
      NisResp := '12345678901';
      NrOc := '51561561';
      ufOC := ufSP;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2241;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2241.Add do
  begin
    EvtInsApo.Sequencial := 0;

    EvtInsApo.IdeEvento.indRetif := ireOriginal;
    EvtInsApo.IdeEvento.NrRecibo := '654654865656';
    EvtInsApo.IdeEvento.TpAmb := taProducaoRestrita;
    EvtInsApo.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtInsApo.IdeEvento.VerProc := '1.0';

    EvtInsApo.IdeEmpregador.TpInsc := tiCNPJ;
    EvtInsApo.IdeEmpregador.NrInsc := '12345678901234';

    EvtInsApo.IdeVinculo.CpfTrab := '12345678901';
    EvtInsApo.IdeVinculo.NisTrab := '12345678901';
    EvtInsApo.IdeVinculo.Matricula := '564545';

    // InsalPeric - Informações de insalubridade e periculosidade
    EvtInsApo.InsalPeric.iniInsalPeric.DtiniCondicao := date - 60;
    with EvtInsApo.InsalPeric.iniInsalPeric.InfoAmb.Add do
    begin
      codAmb := '654';
      InfoAtiv.dscAtivDes := 'dscAtivDes';
      with FatRisco.Add do
        codFatRis := '1234567890';
    end;

    // Opcional - usado para alterações nas condições de trabalho previamente informadas
    // so sera enviado posteriormente quando for alterar um registro
    (* EvtInsApo.InsalPeric.altInsalPeric.DtaltCondicao := Date;
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
      end; *)

    // Opcional - usado quando cessarem as condições de trabalho previamente informadas
    // EvtInsApo.InsalPeric.fimInsalPeric.DtfimCondicao := Date;
    // EvtInsApo.InsalPeric.fimInsalPeric.InfoAmb.codAmb := '123456';

    // AposentEsp - Infomações de condições que ensejam aposentadoria especial
    EvtInsApo.AposentEsp.iniAposentEsp.DtiniCondicao := date - 60;
    with EvtInsApo.AposentEsp.iniAposentEsp.InfoAmb.Add do
    begin
      codAmb := '654';
      InfoAtiv.dscAtivDes := 'dscAtivDes';
      with FatRisco.Add do
        codFatRis := '1234567890';
    end;

    // Opcional - usado para alterações nas condições de trabalho previamente informadas
    // so sera enviado posteriormente quando for alterar um registro
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

    // Opcional - usado quando cessarem as condições de trabalho previamente informadas
    // EvtInsApo.AposentEsp.fimAposentEsp.DtfimCondicao := Date;
    // EvtInsApo.AposentEsp.fimAposentEsp.InfoAmb.codAmb := '654321';
  end;
end;

procedure TFExemploEsocial.GerareSocial2250;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2250.Add do
  begin
    EvtAvPrevio.Sequencial := 0;

    EvtAvPrevio.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtAvPrevio.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtAvPrevio.IdeEvento.TpAmb := taProducaoRestrita;
    EvtAvPrevio.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtAvPrevio.IdeEvento.VerProc := '1.0';

    EvtAvPrevio.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtAvPrevio.IdeEmpregador.NrInsc := '12345678901234';

    EvtAvPrevio.IdeVinculo.CpfTrab := '12345678901';
    EvtAvPrevio.IdeVinculo.NisTrab := '12345678901';
    EvtAvPrevio.IdeVinculo.Matricula := '123456';

    // aviso
    if cbAviso.ItemIndex = 0 then
    begin
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.dtAvPrv := Now;
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.dtPrevDeslig := Now + 30;
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.tpAvPrevio := tpTpAvPrevio(0);
      EvtAvPrevio.InfoAvPrevio.DetAvPrevio.observacao :=
        'Observacao aviso previo';
    end
    else // cancelamento aviso
    begin
      EvtAvPrevio.InfoAvPrevio.CancAvPrevio.dtCancAvPrv := Now;
      EvtAvPrevio.InfoAvPrevio.CancAvPrevio.mtvCancAvPrevio :=
        tpMtvCancAvPrevio(0);
      EvtAvPrevio.InfoAvPrevio.CancAvPrevio.observacao :=
        'Observacao cancelamento aviso previo';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2298;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2298.Add do
  begin
    EvtReintegr.Sequencial := 0;

    EvtReintegr.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtReintegr.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtReintegr.IdeEvento.TpAmb := taProducaoRestrita;
    EvtReintegr.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtReintegr.IdeEvento.VerProc := '1.0';

    EvtReintegr.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtReintegr.IdeEmpregador.NrInsc := '0123456789';

    EvtReintegr.IdeVinculo.CpfTrab := '12345678901';
    EvtReintegr.IdeVinculo.NisTrab := '88888888888';
    EvtReintegr.IdeVinculo.Matricula := '123456';

    EvtReintegr.InfoReintegr.tpReint := tpTpReint(0);
    EvtReintegr.InfoReintegr.nrProcJud := '999999999';
    EvtReintegr.InfoReintegr.dtEfetRetorno := Now + 20;
    EvtReintegr.InfoReintegr.dtEfeito := Now;
    EvtReintegr.InfoReintegr.indPagtoJuizo := tpSim;
  end;
end;

procedure TFExemploEsocial.GerareSocial2299;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2299.Add do
  begin
    EvtDeslig.Sequencial := 0;

    EvtDeslig.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtDeslig.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtDeslig.IdeEvento.TpAmb := taProducaoRestrita;
    EvtDeslig.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtDeslig.IdeEvento.VerProc := '1.0';

    EvtDeslig.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtDeslig.IdeEmpregador.NrInsc := '0123456789';

    EvtDeslig.IdeVinculo.CpfTrab := '33333333303';
    EvtDeslig.IdeVinculo.NisTrab := '11111111111';
    EvtDeslig.IdeVinculo.Matricula := '123456';

    EvtDeslig.InfoDeslig.mtvDeslig := '02';
    EvtDeslig.InfoDeslig.DtDeslig := date;
    EvtDeslig.InfoDeslig.indPagtoAPI := tpSim;
    EvtDeslig.InfoDeslig.dtProjFimAPI := Now;
    EvtDeslig.InfoDeslig.pensAlim := paPercentualeValordePensaoAlimenticia;
    EvtDeslig.InfoDeslig.percAliment := 25.5;
    EvtDeslig.InfoDeslig.vrAlim := 1985.65;

    // Certidão de óbito apenas em caso de morte quando o mtvDeslig for 09 ou 10
    EvtDeslig.InfoDeslig.nrCertObito := '0123456789';

    // numero do processo que decidiu o desligamento mtvdeslig = 17
    EvtDeslig.InfoDeslig.nrProcTrab := '9632587410';

    EvtDeslig.InfoDeslig.indCumprParc := cpaCumprimentoTotal;

    // Obsercação opcional
    EvtDeslig.InfoDeslig.observacao :=
      'Anotações relevantes sobre o desligamento que não tenham campo próprio';

    EvtDeslig.InfoDeslig.SucessaoVinc.cnpjEmpregAnt := '12345678912345';
    // Corrigir nome do campo ou mudar classe.

    with EvtDeslig.InfoDeslig.VerbasResc.dmDev.Add do
    begin
      ideDmDev := '1234567890';
      with infoPerApur.ideEstabLot.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '12345678901234';
        codLotacao := 'A1234';
        with detVerbas.Add do
        begin
          CodRubr := 'Pg123';
          ideTabRubr := 'A01';
          qtdRubr := 2;
          fatorRubr := 200.65;
          vrUnit := 152.35;
          vrRubr := 304.70;
        end;
        with infoSaudeColet.detOper.Add do
        begin
          cnpjOper := '74563215000195';
          regANS := '123456';
          vrPgTit := 150.65;
          with detPlano.Add do
          begin
            cpfDep := '11111111111';
            nmDep := 'Nome do dependente';
            DtNascto := Now;
            vlrPgDep := 150.84;
          end;
        end;
        infoAgNocivo.grauExp := ge1;
        infoSimples.indSimples := idsIntegralmente;
      end;

      with infoPerAnt.ideADC.Add do
      begin
        dtAcConv := Now;
        tpAcConv := tacAcordoColTrab;
        dtEfAcConv := Now;
        dsc := 'Detalhamento';
        with IdePeriodo.Add do
        begin
          perRef := '2017-05';
          with ideEstabLot.Add do
          begin
            TpInsc := tiCNPJ;
            NrInsc := '12345678901234';
            codLotacao := 'A1234';
            with detVerbas.Add do
            begin
              CodRubr := 'Pg124';
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

    with EvtDeslig.InfoDeslig.VerbasResc.procJudTrab.Add do
    begin
      tpTrib := tptIRRF;
      nrProcJud := '0123654789';
      codSusp := 1235;
    end;

    EvtDeslig.InfoDeslig.VerbasResc.infoMV.indMV := tpIndMV(0);
    with EvtDeslig.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Add do
    begin
      TpInsc := tiCNPJ;
      NrInsc := '12345678901234';
      CodCateg := 123;
      vlrRemunOE := 500.84;
    end;

    EvtDeslig.InfoDeslig.Quarentena.dtFimQuar := Now;

    EvtDeslig.InfoDeslig.consigFGTS.idConsig := tpSim;
    EvtDeslig.InfoDeslig.consigFGTS.insConsig := '12345';
    EvtDeslig.InfoDeslig.consigFGTS.nrContr := '123456';
  end;
end;

procedure TFExemploEsocial.GerareSocial2300;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2300.Add do
  begin
    EvtTSVInicio.Sequencial := 0;

    EvtTSVInicio.IdeEvento.TpAmb := taProducaoRestrita;
    EvtTSVInicio.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtTSVInicio.IdeEvento.VerProc := '1.0';

    EvtTSVInicio.IdeEmpregador.TpInsc := tiCNPJ;
    EvtTSVInicio.IdeEmpregador.NrInsc := '12345678987654';

    EvtTSVInicio.Trabalhador.CpfTrab := '98765432123';
    EvtTSVInicio.Trabalhador.NisTrab := '54789632145';
    EvtTSVInicio.Trabalhador.NmTrab := 'João das Neve';
    EvtTSVInicio.Trabalhador.Sexo := 'M';
    EvtTSVInicio.Trabalhador.RacaCor := 1;
    EvtTSVInicio.Trabalhador.EstCiv := 1;
    EvtTSVInicio.Trabalhador.GrauInstr := '07';
    EvtTSVInicio.Trabalhador.nmSoc := 'Nome social';

    EvtTSVInicio.Trabalhador.Nascimento.DtNascto := date;
    EvtTSVInicio.Trabalhador.Nascimento.codMunic := 4153623;
    EvtTSVInicio.Trabalhador.Nascimento.uf := 'PR';
    EvtTSVInicio.Trabalhador.Nascimento.PaisNascto := '063';
    EvtTSVInicio.Trabalhador.Nascimento.PaisNac := '105';
    EvtTSVInicio.Trabalhador.Nascimento.NmMae := 'Joana das Neve';
    EvtTSVInicio.Trabalhador.Nascimento.NmPai := 'Jose das Neve';

    EvtTSVInicio.Trabalhador.Documentos.CTPS.NrCtps := '01234567897';
    EvtTSVInicio.Trabalhador.Documentos.CTPS.SerieCtps := '00001';
    EvtTSVInicio.Trabalhador.Documentos.CTPS.UfCtps := 'PR';

    EvtTSVInicio.Trabalhador.Documentos.RIC.NrRic := '10203040506070';
    EvtTSVInicio.Trabalhador.Documentos.RIC.OrgaoEmissor := 'Orgão Emissor';
    EvtTSVInicio.Trabalhador.Documentos.RIC.DtExped := date;

    EvtTSVInicio.Trabalhador.Documentos.RG.NrRg := '73062584';
    EvtTSVInicio.Trabalhador.Documentos.RG.OrgaoEmissor := 'SSP';
    EvtTSVInicio.Trabalhador.Documentos.RG.DtExped := date;

    EvtTSVInicio.Trabalhador.Documentos.RNE.NrRne := '01020304050607';
    EvtTSVInicio.Trabalhador.Documentos.RNE.OrgaoEmissor := 'Orgao Emissor';
    EvtTSVInicio.Trabalhador.Documentos.RNE.DtExped := date;

    EvtTSVInicio.Trabalhador.Documentos.OC.NrOc := '74108520963012';
    EvtTSVInicio.Trabalhador.Documentos.OC.OrgaoEmissor := 'CRProfissao';
    EvtTSVInicio.Trabalhador.Documentos.OC.DtExped := date;
    EvtTSVInicio.Trabalhador.Documentos.OC.DtValid := date;

    EvtTSVInicio.Trabalhador.Documentos.CNH.nrRegCnh := '123654789632';
    EvtTSVInicio.Trabalhador.Documentos.CNH.DtExped := date;
    EvtTSVInicio.Trabalhador.Documentos.CNH.ufCnh := ufPR;
    EvtTSVInicio.Trabalhador.Documentos.CNH.DtValid := date;
    EvtTSVInicio.Trabalhador.Documentos.CNH.dtPriHab := date;
    EvtTSVInicio.Trabalhador.Documentos.CNH.categoriaCnh := cnAB;

    EvtTSVInicio.Trabalhador.Endereco.Brasil.TpLograd := 'R';
    EvtTSVInicio.Trabalhador.Endereco.Brasil.DscLograd := 'Rua Parmenides';
    EvtTSVInicio.Trabalhador.Endereco.Brasil.NrLograd := '123456';
    EvtTSVInicio.Trabalhador.Endereco.Brasil.Complemento := 'fundos';
    EvtTSVInicio.Trabalhador.Endereco.Brasil.Bairro := 'Jd Filosofia';
    EvtTSVInicio.Trabalhador.Endereco.Brasil.Cep := '88888888';
    EvtTSVInicio.Trabalhador.Endereco.Brasil.codMunic := 4141414;
    EvtTSVInicio.Trabalhador.Endereco.Brasil.uf := ufPR;

    // Dados de trabalhador estrangeiro
    EvtTSVInicio.Trabalhador.Endereco.Exterior.PaisResid := '063';
    EvtTSVInicio.Trabalhador.Endereco.Exterior.DscLograd := 'St. Abbey Road';
    EvtTSVInicio.Trabalhador.Endereco.Exterior.NrLograd := '123456';
    EvtTSVInicio.Trabalhador.Endereco.Exterior.Complemento := 'apto 010';
    EvtTSVInicio.Trabalhador.Endereco.Exterior.Bairro := 'RubberSoul';
    EvtTSVInicio.Trabalhador.Endereco.Exterior.NmCid := 'Buenos Aires';
    EvtTSVInicio.Trabalhador.Endereco.Exterior.CodPostal := '987654';

    EvtTSVInicio.Trabalhador.TrabEstrangeiro.DtChegada := date;
    EvtTSVInicio.Trabalhador.TrabEstrangeiro.ClassTrabEstrang :=
      ctVistoPermanente;
    EvtTSVInicio.Trabalhador.TrabEstrangeiro.CasadoBr := 'S';
    EvtTSVInicio.Trabalhador.TrabEstrangeiro.FilhosBr := 'N';

    // Dados de trabalhador com deficiencia
    EvtTSVInicio.Trabalhador.InfoDeficiencia.DefFisica := tpNao;
    EvtTSVInicio.Trabalhador.InfoDeficiencia.DefVisual := tpNao;
    EvtTSVInicio.Trabalhador.InfoDeficiencia.DefAuditiva := tpNao;
    EvtTSVInicio.Trabalhador.InfoDeficiencia.DefMental := tpNao;
    EvtTSVInicio.Trabalhador.InfoDeficiencia.DefIntelectual := tpNao;
    EvtTSVInicio.Trabalhador.InfoDeficiencia.ReabReadap := tpSim;
    EvtTSVInicio.Trabalhador.InfoDeficiencia.observacao := 'sem deficiencia';

    with EvtTSVInicio.Trabalhador.Dependente.Add do
    begin
      tpDep := tdConjuge;
      nmDep := 'Dependente 1';
      DtNascto := date;
      cpfDep := '99999999909';
      depIRRF := tpSim;
      depSF := tpNao;
    end;

    with EvtTSVInicio.Trabalhador.Dependente.Add do
    begin
      tpDep := tdFilhoOuEnteado;
      nmDep := 'Dependente 2';
      DtNascto := date;
      cpfDep := '99999999909';
      depIRRF := tpSim;
      depSF := tpNao;
    end;

    EvtTSVInicio.Trabalhador.Contato.FonePrinc := '91067240';
    EvtTSVInicio.Trabalhador.Contato.FoneAlternat := '91067240';
    EvtTSVInicio.Trabalhador.Contato.EmailPrinc := 'TESTE@email.com.br';
    EvtTSVInicio.Trabalhador.Contato.EmailAlternat := 'teste@teste.com';

    EvtTSVInicio.infoTSVInicio.CodCateg := 101;
    EvtTSVInicio.infoTSVInicio.dtInicio := date;
    EvtTSVInicio.infoTSVInicio.natAtivididade := navUrbano;

    EvtTSVInicio.infoTSVInicio.InfoComplementares.cargoFuncao.CodCargo := '001';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.cargoFuncao.CodFuncao
      := '001';

    EvtTSVInicio.infoTSVInicio.InfoComplementares.Remuneracao.VrSalFx
      := 1200.00;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.Remuneracao.UndSalFixo
      := sfPorMes;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.Remuneracao.DscSalVar :=
      'Comissão de 1,2% sobre a venda do mês';

    EvtTSVInicio.infoTSVInicio.InfoComplementares.FGTS.OpcFGTS := ofOptante;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.FGTS.DtOpcFGTS := date;

    // dados da empresa de origem do dirigente sindical
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoDirSind.categOrig := 111;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoDirSind.cnpjOrigem :=
      '12345678901234';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoDirSind.dtAdmOrig := date;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoDirSind.matricOrig
      := 'A1234';

    // Informações de trabalhador cedido, devem ser preenchidas exclusivamente pelo cessionário
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.
      categOrig := 111;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.cnpjCednt :=
      '12345678901234';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.matricCed
      := 'B4321';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.
      dtAdmCed := date;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.
      TpRegTrab := trCLT;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.TpRegPrev
      := rpRGPS;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoTrabCedido.infOnus :=
      ocCedente;

    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.natEstagio :=
      neObrigatiorio;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.nivEstagio :=
      nvSuperior;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.areaAtuacao :=
      'Direito';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.nrApol
      := '123456';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.vlrBolsa
      := 600.00;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.dtPrevTerm :=
      IncMonth(date, 12);

    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      cnpjInstEnsino := '12345678901234';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      NmRazao := 'Nome da Instituição de Ensino';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      DscLograd := 'R Pitagoras';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      NrLograd := '1618';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      Bairro := 'Bairro Educacional';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.Cep
      := '86086086';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      codMunic := 4141414;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.instEnsino.
      uf := ufPR;

    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      cnpjAgntInteg := '12345678901234';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      NmRazao := 'Nome da empresa';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      DscLograd := 'R Adam Smith';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      NrLograd := '9999';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      Bairro := 'Bairro Empresarial';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      Cep := '86086086';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      codMunic := 4141414;
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.ageIntegracao.
      uf := ufPR;

    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.
      supervisorEstagio.cpfSupervisor := '88888888801';
    EvtTSVInicio.infoTSVInicio.InfoComplementares.infoEstagiario.
      supervisorEstagio.nmSuperv := 'Pedro das Pedras';
  end;
end;

procedure TFExemploEsocial.GerareSocial2306;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2306.Add do
  begin
    EvtTSVAltContr.Sequencial := 0;

    EvtTSVAltContr.IdeEvento.TpAmb := taProducaoRestrita;
    EvtTSVAltContr.IdeEvento.ProcEmi := peAplicEmpregador;
    EvtTSVAltContr.IdeEvento.VerProc := '1.0';

    EvtTSVAltContr.IdeEmpregador.TpInsc := tiCNPJ;
    EvtTSVAltContr.IdeEmpregador.NrInsc := '12345678987654';

    EvtTSVAltContr.IdeTrabSemVinc.CpfTrab := '12345678901';
    EvtTSVAltContr.IdeTrabSemVinc.NisTrab := '00000000000';
    EvtTSVAltContr.IdeTrabSemVinc.CodCateg := 555;

    EvtTSVAltContr.infoTSVAlteracao.dtAlteracao := date;
    EvtTSVAltContr.infoTSVAlteracao.natAtivididade := navUrbano;

    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.cargoFuncao.
      CodCargo := '001';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.cargoFuncao.
      CodFuncao := '001';

    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.Remuneracao.VrSalFx
      := 1200.00;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.Remuneracao.UndSalFixo
      := sfPorMes;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.Remuneracao.DscSalVar :=
      'Comissão de 1,2% sobre a venda do mês';

    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.natEstagio
      := neObrigatiorio;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.nivEstagio
      := nvSuperior;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      areaAtuacao := 'Direito';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.nrApol
      := '123456';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.vlrBolsa
      := 600.00;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.dtPrevTerm
      := IncMonth(date, 12);

    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.cnpjInstEnsino := '12345678998765';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.NmRazao := 'Nome da Instituição de Ensino';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.DscLograd := 'R Pitagoras';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.NrLograd := '1618';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.Bairro := 'Bairro Educacional';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.Cep := '86086086';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.codMunic := 4141414;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      instEnsino.uf := ufPR;

    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.cnpjAgntInteg := '98765432145678';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.NmRazao := 'Nome da empresa';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.DscLograd := 'R Adam Smith';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.NrLograd := '9999';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.Bairro := 'Bairro Empresarial';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.Cep := '86086086';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.codMunic := 4141414;
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      ageIntegracao.uf := ufPR;

    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      supervisorEstagio.cpfSupervisor := '12345678901';
    EvtTSVAltContr.infoTSVAlteracao.InfoComplementares.infoEstagiario.
      supervisorEstagio.nmSuperv := 'Pedro das Pedras';
  end;
end;

procedure TFExemploEsocial.GerareSocial2399;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2399.Add do
  begin
    EvtTSVTermino.Sequencial := 0;

    EvtTSVTermino.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtTSVTermino.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtTSVTermino.IdeEvento.TpAmb := taProducaoRestrita;
    EvtTSVTermino.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtTSVTermino.IdeEvento.VerProc := '1.0';

    EvtTSVTermino.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtTSVTermino.IdeEmpregador.NrInsc := '0123456789';

    EvtTSVTermino.IdeTrabSemVinc.CpfTrab := '12345678987';
    EvtTSVTermino.IdeTrabSemVinc.NisTrab := '98765432123';
    EvtTSVTermino.IdeTrabSemVinc.CodCateg := 111;

    EvtTSVTermino.InfoTSVTermino.dtTerm := date;

    EvtTSVTermino.InfoTSVTermino.mtvDesligTSV := '02';

    with EvtTSVTermino.InfoTSVTermino.VerbasResc.dmDev.Add do
    begin
      ideDmDev := '012345';
      with ideEstabLot.Add do
      begin
        TpInsc := tiCNPJ;
        NrInsc := '12345678987654';
        codLotacao := 'A1234';
        with detVerbas.Add do
        begin
          CodRubr := 'Pg123';
          ideTabRubr := 'A01';
          qtdRubr := 2;
          fatorRubr := 50.25;
          vrUnit := 152.35;
          vrRubr := 304.70;
        end;
        with infoSaudeColet.detOper.Add do
        begin
          cnpjOper := '89652048000195';
          regANS := '123456';
          vrPgTit := 1500.65;
          with detPlano.Add do
          begin
            cpfDep := '55555555555';
            nmDep := 'Nome do Dependente';
            DtNascto := Now;
            vlrPgDep := 125.36;
          end;
        end;
        infoAgNocivo.grauExp := ge1;
        infoSimples.indSimples := idsIntegralmente;
      end;
    end;
    with EvtTSVTermino.InfoTSVTermino.VerbasResc.procJudTrab.Add do
    begin
      tpTrib := tpTpTributo(0);
      nrProcJud := '123456789';
      codSusp := 123456;
    end;
    EvtTSVTermino.InfoTSVTermino.VerbasResc.infoMV.indMV := tpIndMV(0);
    with EvtTSVTermino.InfoTSVTermino.VerbasResc.infoMV.remunOutrEmpr.Add do
    begin
      TpInsc := tiCNPJ;
      NrInsc := '14236547000195';
      CodCateg := 111;
      vlrRemunOE := 2500.12;
    end;

    EvtTSVTermino.InfoTSVTermino.Quarentena.dtFimQuar := date;
  end;
end;

procedure TFExemploEsocial.GerareSocial2400;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2400.Add do
  begin
    EvtCdBenPrRP.Sequencial := 0;

    EvtCdBenPrRP.IdeEvento.indRetif := tpIndRetificacao(0);
    EvtCdBenPrRP.IdeEvento.NrRecibo := '65.5454.987798798798';
    EvtCdBenPrRP.IdeEvento.TpAmb := taProducaoRestrita;
    EvtCdBenPrRP.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtCdBenPrRP.IdeEvento.VerProc := '1.0';

    EvtCdBenPrRP.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtCdBenPrRP.IdeEmpregador.NrInsc := '0123456789';

    EvtCdBenPrRP.ideBenef.cpfBenef := '12345678910';
    EvtCdBenPrRP.ideBenef.nmBenefic := 'Nome do beneficiario';

    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.DtNascto := date;;
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.codMunic := 4153623;
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.uf := 'PR';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.PaisNascto := '063';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.PaisNac := '105';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.NmMae := 'Joana das Neve';
    EvtCdBenPrRP.ideBenef.dadosBenef.dadosNasc.NmPai := 'Jose das Neve';

    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.TpLograd := 'R';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.DscLograd :=
      'Rua Parmenides';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.NrLograd := '123456';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.Complemento := 'fundos';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.Bairro := 'Jd Filosofia';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.Cep := '88888888';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.codMunic := 4141414;
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Brasil.uf := ufPR;

    // Dados de trabalhador estrangeiro
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.PaisResid := '063';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.DscLograd :=
      'St. Abbey Road';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.NrLograd := '123456';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.Complemento :=
      'apto 010';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.Bairro := 'RubberSoul';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.NmCid := 'Buenos Aires';
    EvtCdBenPrRP.ideBenef.dadosBenef.Endereco.Exterior.CodPostal := '987654';

    EvtCdBenPrRP.infoBeneficio.tpPlanRP := prpPlanoPrevidenciarioOuUnico;

    EvtCdBenPrRP.infoBeneficio.iniBeneficio.tpBenef := 1;
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.nrBenefic := '3156189132131';
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.dtIniBenef := Now;
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.vrBenef := 1500.32;
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.infoPenMorte.idQuota :=
      '1521651651';
    EvtCdBenPrRP.infoBeneficio.iniBeneficio.infoPenMorte.cpfInst :=
      '12345678910';

    EvtCdBenPrRP.infoBeneficio.altBeneficio.tpBenef := 1;
    EvtCdBenPrRP.infoBeneficio.altBeneficio.nrBenefic := '3156189132131';
    EvtCdBenPrRP.infoBeneficio.altBeneficio.dtIniBenef := Now;
    EvtCdBenPrRP.infoBeneficio.altBeneficio.vrBenef := 1500.32;
    EvtCdBenPrRP.infoBeneficio.altBeneficio.infoPenMorte.idQuota :=
      '1521651651';
    EvtCdBenPrRP.infoBeneficio.altBeneficio.infoPenMorte.cpfInst :=
      '12345678910';

    EvtCdBenPrRP.infoBeneficio.fimBeneficio.tpBenef := 1;
    EvtCdBenPrRP.infoBeneficio.fimBeneficio.nrBenefic := '3156189132131';
    EvtCdBenPrRP.infoBeneficio.fimBeneficio.dtFimBenef := Now;
    EvtCdBenPrRP.infoBeneficio.fimBeneficio.mtvFim := 1;

  end;
end;

procedure TFExemploEsocial.GerareSocial3000;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S3000.Add do
  begin
    EvtExclusao.Sequencial := 0;

    EvtExclusao.IdeEvento.TpAmb := taProducaoRestrita;
    EvtExclusao.IdeEvento.ProcEmi := TpProcEmi(0);
    EvtExclusao.IdeEvento.VerProc := '1.0';

    EvtExclusao.IdeEmpregador.TpInsc := tpTpInsc(1);
    EvtExclusao.IdeEmpregador.NrInsc := '0123456789';

    EvtExclusao.InfoExclusao.tpEvento := TTipoEvento(teS2100);
    EvtExclusao.InfoExclusao.nrRecEvt := '12345789987654321';

    EvtExclusao.InfoExclusao.ideTrabalhador.CpfTrab := '12345678950';
    EvtExclusao.InfoExclusao.ideTrabalhador.NisTrab := '12345678901';

    EvtExclusao.InfoExclusao.IdeFolhaPagto.IndApuracao := tpIndApuracao(0);
    EvtExclusao.InfoExclusao.IdeFolhaPagto.perApur := '2015-05';
  end;
end;

procedure TFExemploEsocial.btnGerarClick(Sender: TObject);
begin
  SelecionaEventos;

  ACBreSocial1.Eventos.GerarXMLs;
  ACBreSocial1.Eventos.SaveToFiles;

  MemoResp.Lines.Add('XML de Eventos Gerados com Sucesso!');
  pgWebservice.ActivePageIndex := 3;
end;

procedure TFExemploEsocial.btnEnviarClick(Sender: TObject);
var
  I: Integer;
begin
  if chkClear.Checked then
    LimparDocsPasta;

  try
//    SelecionaEventos;
//    ACBreSocial1.AssinarEventos;

    ACBreSocial1.Enviar(TESocialGrupo(rdgGrupo.ItemIndex + 1));
    Sleep(3000);

    MemoResp.Lines.Text := ACBreSocial1.WebServices.EnvioLote.RetWS;
    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.EnvioLote.RetEnvioLote do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);

        if Status.cdResposta in [201, 202] then
        begin
          Add('ideEmpregador');
          Add(' - TpInsc: ' + eSTpInscricaoToStr(IdeEmpregador.TpInsc));
          Add(' - NrInsc: ' + IdeEmpregador.NrInsc);
          Add('ideTransmissor');
          Add(' - TpInsc: ' + eSTpInscricaoToStr(IdeTransmissor.TpInsc));
          Add(' - NrInsc: ' + IdeTransmissor.NrInsc);
          Add('dadosRecepcaoLote');
          Add(' - dhRecepcao..............: ' + DateTimeToStr(dadosRecLote.dhRecepcao));
          Add(' - versaoAplicativoRecepcao: ' + dadosRecLote.versaoAplicRecepcao);
          Add(' - protocoloEnvio..........: ' + dadosRecLote.Protocolo);
        end
        else
        begin
          for I := 0 to Status.Ocorrencias.Count - 1 do
          begin
            with Status.Ocorrencias.Items[I] do
            begin
              Add(' Ocorrencia ' + IntToStr(I));
              Add('   Código.....: ' + IntToStr(Codigo));
              Add('   Descrição..: ' + Descricao);
              Add('   Tipo.......: ' + IntToStr(Tipo));
              Add('   Localização: ' + Localizacao);
            end;
          end;
        end;
      end;
    end;

    pgWebservice.ActivePageIndex := 3;
  finally
    ACBreSocial1.Eventos.Clear;
  end;
end;

procedure TFExemploEsocial.btnConsultarClick(Sender: TObject);
var
  Protocolo: string;
  I, J: Integer;
begin
  Protocolo := '';
  if not(InputQuery('WebServices: Consulta Protocolo', 'Protocolo', Protocolo)) then
    Exit;

  if ACBreSocial1.Consultar(Protocolo) then
  begin

    MemoResp.Lines.Text := ACBreSocial1.WebServices.ConsultaLote.RetWS;

    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.ConsultaLote.RetConsultaLote do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);

        if Status.cdResposta in [201, 202] then
        begin
          Add('ideEmpregador');
          Add(' - TpInsc: ' + eSTpInscricaoToStr(IdeEmpregador.TpInsc));
          Add(' - NrInsc: ' + IdeEmpregador.NrInsc);
          Add('ideTransmissor');
          Add(' - TpInsc: ' + eSTpInscricaoToStr(IdeTransmissor.TpInsc));
          Add(' - NrInsc: ' + IdeTransmissor.NrInsc);
          Add('dadosRecepcaoLote');
          Add(' - dhRecepcao..............: ' + DateTimeToStr(dadosRecLote.dhRecepcao));
          Add(' - versaoAplicativoRecepcao: ' + dadosRecLote.versaoAplicRecepcao);
          Add(' - protocoloEnvio..........: ' + dadosRecLote.Protocolo);

          for I := 0 to retEventos.Count - 1 do
          begin
            Add('Processamento');
            Add(' - cdResposta.........: ' + IntToStr(retEventos.Items[I].Processamento.cdResposta));
            Add(' - descResposta.......: ' + retEventos.Items[I].Processamento.descResposta);
            Add(' - versaoAplicProcLote: ' + retEventos.Items[I].Processamento.versaoAplicProcLote);
            Add(' - dhProcessamento....: ' + DateTimeToStr(retEventos.Items[I].Processamento.dhProcessamento));

            if retEventos.Items[I].Processamento.Ocorrencias.Count > 0 then
            begin
              Add('Ocorrencias do Processamento');
              for J := 0 to retEventos.Items[I].Processamento.Ocorrencias.Count - 1 do
              begin
                Add(' Ocorrencia ' + IntToStr(J));
                Add('   Código.....: ' + IntToStr(retEventos.Items[I].Processamento.Ocorrencias.Items[J].Codigo));
                Add('   Descrição..: ' + retEventos.Items[I].Processamento.Ocorrencias.Items[J].Descricao);
                Add('   Tipo.......: ' + IntToStr(retEventos.Items[I].Processamento.Ocorrencias.Items[J].Tipo));
                Add('   Localização: ' + retEventos.Items[I].Processamento.Ocorrencias.Items[J].Localizacao);
              end;
            end;

            Add('Recibo');
            Add(' - nrRecibo: ' + retEventos.Items[I].Recibo.nrRecibo);
            Add(' - hash....: ' + retEventos.Items[I].Recibo.hash);
          end;

        end
        else
        begin
          for I := 0 to Status.Ocorrencias.Count - 1 do
          begin
            with Status.Ocorrencias.Items[I] do
            begin
              Add(' Ocorrencia ' + IntToStr(I));
              Add('   Código.....: ' + IntToStr(Codigo));
              Add('   Descrição..: ' + Descricao);
              Add('   Tipo.......: ' + IntToStr(Tipo));
              Add('   Localização: ' + Localizacao);
            end;
          end;
        end;
      end;
    end;

    pgWebservice.ActivePageIndex := 3;
  end;
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
      ACBreSocial1.Configuracoes.Geral.SSLCryptLib :=
        TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFExemploEsocial.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLHttpLib :=
        TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFExemploEsocial.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib :=
        TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
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
  OpenDialog1.Filter :=
    'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(Application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtCaminho.Text := OpenDialog1.FileName;
  end;
end;

procedure TFExemploEsocial.sbtnListaCertClick(Sender: TObject);
var
  i: Integer;
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
      Cells[0, 0] := 'Num.Série';
      Cells[1, 0] := 'Razão Social';
      Cells[2, 0] := 'CNPJ';
      Cells[3, 0] := 'Validade';
      Cells[4, 0] := 'Certificadora';
    end;

    for i := 0 to ACBreSocial1.SSL.ListaCertificados.Count - 1 do
    begin
      with ACBreSocial1.SSL.ListaCertificados[i] do
      begin
        ASerie := NumeroSerie;
        if (CNPJ <> '') then
        begin
          with frSelecionarCertificado.StringGrid1 do
          begin
            if AddRow then
              RowCount := RowCount + 1;

            Cells[0, RowCount - 1] := NumeroSerie;
            Cells[1, RowCount - 1] := RazaoSocial;
            Cells[2, RowCount - 1] := CNPJ;
            Cells[3, RowCount - 1] := FormatDateBr(DataVenc);
            Cells[4, RowCount - 1] := Certificadora;
            AddRow := True;
          end;
        end;
      end;
    end;

    frSelecionarCertificado.ShowModal;

    if frSelecionarCertificado.ModalResult = mrOK then
      edtNumSerie.Text := frSelecionarCertificado.StringGrid1.Cells
        [0, frSelecionarCertificado.StringGrid1.Row];

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
  ShowMessage(FormatDateBr(ACBreSocial1.SSL.CertDataVenc));
end;

procedure TFExemploEsocial.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertNumeroSerie);
end;

procedure TFExemploEsocial.btnSubjectNameClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertSubjectName + sLineBreak + sLineBreak +
    'Razão Social: ' + ACBreSocial1.SSL.CertRazaoSocial);
end;

procedure TFExemploEsocial.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertCNPJ);
end;

procedure TFExemploEsocial.btnIssuerNameClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertIssuerName + sLineBreak + sLineBreak +
    'Certificadora: ' + ACBreSocial1.SSL.CertCertificadora);
end;

procedure TFExemploEsocial.btnSHA_RSAClick(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBreSocial1.SSL.CalcHash(edtCalcHash.Text, dgstSHA256, outBase64,
    cbAssinar.Checked);
  MemoResp.Lines.Add(Ahash);
  pgWebservice.ActivePageIndex := 3;
end;

procedure TFExemploEsocial.btnHTTPSClick(Sender: TObject);
var
  Acao: String;
  OldUseCert: Boolean;
begin
  Acao := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>' +
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" '
    + 'xmlns:cli="http://cliente.bean.master.sigep.bsb.correios.com.br/"> ' +
    ' <soapenv:Header/>' + ' <soapenv:Body>' + ' <cli:consultaCEP>' +
    ' <cep>18270-170</cep>' + ' </cli:consultaCEP>' + ' </soapenv:Body>' +
    ' </soapenv:Envelope>';

  OldUseCert := ACBreSocial1.SSL.UseCertificateHTTP;
  ACBreSocial1.SSL.UseCertificateHTTP := False;
  try
    MemoResp.Lines.Text := ACBreSocial1.SSL.Enviar(Acao,
      'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl',
      '');
  finally
    ACBreSocial1.SSL.UseCertificateHTTP := OldUseCert;
  end;
  pgWebservice.ActivePageIndex := 3;
end;

procedure TFExemploEsocial.btnX509Click(Sender: TObject);
begin
  with ACBreSocial1.SSL do
  begin
    CarregarCertificadoPublico(MemoXmlEnvio.Lines.Text);
    MemoResp.Lines.Add(CertIssuerName);
    MemoResp.Lines.Add(CertRazaoSocial);
    MemoResp.Lines.Add(CertCNPJ);
    MemoResp.Lines.Add(CertSubjectName);
    MemoResp.Lines.Add(CertNumeroSerie);
    pgWebservice.ActivePageIndex := 3;
  end;
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
  memoLog.Lines.Add(ALogLine);
end;

procedure TFExemploEsocial.ACBreSocial1StatusChange(Sender: TObject);
begin
  case ACBreSocial1.Status of
    stIdle:
      begin
        if (frmStatus <> nil) then
          frmStatus.Hide;
      end;
    stEnvLoteEventos:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Enviando lote do eSocial...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
    stConsultaLote:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Consultando lote do eSocial...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
  end;
  Application.ProcessMessages;
end;

procedure TFExemploEsocial.ACBreSocial1TransmissaoEventos(
  const AXML: String; ATipo: TeSocialEventos);
begin
  case ATipo of
    eseEnvioLote:
      MemoXmlEnvio.Lines.Text := AXML;
    eseRetornoLote:
      MemoXmlRetorno.Lines.Text := AXML;
    eseEnvioConsulta:
      MemoXmlEnvio.Lines.Text := AXML;
    eseRetornoConsulta:
      MemoXmlRetorno.Lines.Text := AXML;
  end;
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
  TLabel(Sender).Font.Style := [fsBold, fsUnderline];
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
  L: TEmpregador;
begin
  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add(GetEnumName(TypeInfo(TSSLLib), Integer(T)));
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add(GetEnumName(TypeInfo(TSSLCryptLib), Integer(U)));
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add(GetEnumName(TypeInfo(TSSLHttpLib), Integer(V)));
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add(GetEnumName(TypeInfo(TSSLXmlSignLib), Integer(X)));
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add(GetEnumName(TypeInfo(TSSLType), Integer(Y)));
  cbSSLType.ItemIndex := 0;

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoeSocial) to High(TVersaoeSocial) do
    cbVersaoDF.Items.Add(GetEnumName(TypeInfo(TVersaoeSocial), Integer(K)));
  // cbVersaoDF.Items[0] := 've240';
  cbVersaoDF.ItemIndex := 0;

  cbTEmpregador.Items.Clear;
  for L := Low(TEmpregador) to High(TEmpregador) do
    cbTEmpregador.Items.Add(GetEnumName(TypeInfo(TEmpregador), Integer(L)));
  cbTEmpregador.ItemIndex := 0;

  LerConfiguracao;
end;

procedure TFExemploEsocial.PathClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(TEdit(Sender).Text) <= 0 then
    Dir := ExtractFileDir(Application.ExeName)
  else
    Dir := TEdit(Sender).Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], SELDIRHELP)
  then
    TEdit(Sender).Text := Dir;
end;

procedure TFExemploEsocial.AtualizaSSLLibsCombo;
begin
  cbSSLLib.ItemIndex := Integer(ACBreSocial1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex := Integer(ACBreSocial1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex := Integer(ACBreSocial1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex :=
    Integer(ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBreSocial1.Configuracoes.Geral.SSLHttpLib
    in [httpWinHttp, httpOpenSSL]);
end;

procedure TFExemploEsocial.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteInteger('Certificado', 'SSLLib', cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib', cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib', cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString('Certificado', 'Caminho', edtCaminho.Text);
    Ini.WriteString('Certificado', 'Senha', edtSenha.Text);
    Ini.WriteString('Certificado', 'NumSerie', edtNumSerie.Text);

    Ini.WriteBool('Geral', 'AtualizarXML', ckSalvar.Checked);
    Ini.WriteBool('Geral', 'ExibirErroSchema', ckSalvar.Checked);
    Ini.WriteString('Geral', 'FormatoAlerta', edtFormatoAlerta.Text);
    Ini.WriteInteger('Geral', 'VersaoDF', cbVersaoDF.ItemIndex);
    Ini.WriteBool('Geral', 'RetirarAcentos', cbxRetirarAcentos.Checked);
    Ini.WriteBool('Geral', 'Salvar', ckSalvar.Checked);
    Ini.WriteString('Geral', 'PathSalvar', edtPathLogs.Text);
    Ini.WriteString('Geral', 'PathSchemas', edtPathSchemas.Text);
    Ini.WriteString('Geral', 'IdEmpregador', edtIdEmpregador.Text);
    Ini.WriteString('Geral', 'IdTransmissor', edtIdTransmissor.Text);
    Ini.WriteInteger('Geral', 'TipoEmpregador', cbTEmpregador.ItemIndex);

    Ini.WriteInteger('WebService', 'Ambiente', rgTipoAmb.ItemIndex);
    Ini.WriteBool('WebService', 'Visualizar', cbxVisualizar.Checked);
    Ini.WriteBool('WebService', 'SalvarSOAP', cbxSalvarSOAP.Checked);
    Ini.WriteBool('WebService', 'AjustarAut', cbxAjustarAut.Checked);
    Ini.WriteString('WebService', 'Aguardar', edtAguardar.Text);
    Ini.WriteString('WebService', 'Tentativas', edtTentativas.Text);
    Ini.WriteString('WebService', 'Intervalo', edtIntervalo.Text);
    Ini.WriteInteger('WebService', 'TimeOut', seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType', cbSSLType.ItemIndex);

    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', edtProxySenha.Text);

    Ini.WriteBool('Arquivos', 'Salvar', cbxSalvarArqs.Checked);
    Ini.WriteBool('Arquivos', 'PastaMensal', cbxPastaMensal.Checked);
    Ini.WriteBool('Arquivos', 'AddLiteral', cbxAdicionaLiteral.Checked);
    Ini.WriteBool('Arquivos', 'EmissaoPatheSocial',
      cbxEmissaoPatheSocial.Checked);
    Ini.WriteBool('Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool('Arquivos', 'SepararPorCNPJ', cbxSepararPorCNPJ.Checked);
    Ini.WriteString('Arquivos', 'PatheSocial', edtPatheSocial.Text);
    Ini.WriteString('Arquivos', 'PathEvento', edtPathEvento.Text);
  finally
    Ini.Free;
  end;
end;

procedure TFExemploEsocial.LerConfiguracao;
var
  IniFile, PathMensal: String;
  Ini: TIniFile;
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

    ACBreSocial1.Configuracoes.Certificados.ArquivoPFX := edtCaminho.Text;
    ACBreSocial1.Configuracoes.Certificados.Senha := edtSenha.Text;
    ACBreSocial1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

    cbxExibirErroSchema.Checked := Ini.ReadBool('Geral',
      'ExibirErroSchema', True);
    edtFormatoAlerta.Text := Ini.ReadString('Geral', 'FormatoAlerta',
      'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbVersaoDF.ItemIndex := Ini.ReadInteger('Geral', 'VersaoDF', 0);
    ckSalvar.Checked := Ini.ReadBool('Geral', 'Salvar', True);
    cbxRetirarAcentos.Checked := Ini.ReadBool('Geral', 'RetirarAcentos', True);
    edtIdEmpregador.Text := Ini.ReadString('Geral', 'IdEmpregador', '');
    edtIdTransmissor.Text := Ini.ReadString('Geral', 'IdTransmissor', '');
    cbTEmpregador.ItemIndex := Ini.ReadInteger('Geral', 'TipoEmpregador', 0);

    ACBreSocial1.SSL.DescarregarCertificado;

    with ACBreSocial1.Configuracoes.Geral do
    begin
      SSLLib := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

      AtualizaSSLLibsCombo;

      ExibirErroSchema := cbxExibirErroSchema.Checked;
      RetirarAcentos := cbxRetirarAcentos.Checked;
      FormatoAlerta := edtFormatoAlerta.Text;
      VersaoDF := TVersaoeSocial(cbVersaoDF.ItemIndex);
      Salvar := ckSalvar.Checked;

      IdEmpregador := edtIdEmpregador.Text;
      IdTransmissor := edtIdTransmissor.Text;
      TipoEmpregador := TEmpregador(cbTEmpregador.ItemIndex);
    end;

    rgTipoAmb.ItemIndex := Ini.ReadInteger('WebService', 'Ambiente', 0);
    cbxVisualizar.Checked := Ini.ReadBool('WebService', 'Visualizar', False);
    cbxSalvarSOAP.Checked := Ini.ReadBool('WebService', 'SalvarSOAP', False);
    cbxAjustarAut.Checked := Ini.ReadBool('WebService', 'AjustarAut', False);
    edtAguardar.Text := Ini.ReadString('WebService', 'Aguardar', '0');
    edtTentativas.Text := Ini.ReadString('WebService', 'Tentativas', '5');
    edtIntervalo.Text := Ini.ReadString('WebService', 'Intervalo', '0');
    seTimeOut.Value := Ini.ReadInteger('WebService', 'TimeOut', 5000);
    cbSSLType.ItemIndex := Ini.ReadInteger('WebService', 'SSLType', 0);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass', '');

    with ACBreSocial1.Configuracoes.WebServices do
    begin
      Ambiente := taHomologacao;
      Visualizar := cbxVisualizar.Checked;
      Salvar := cbxSalvarSOAP.Checked;

      AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

      if NaoEstaVazio(edtAguardar.Text) then
        AguardarConsultaRet := ifThen(StrToInt(edtAguardar.Text) < 1000,
          StrToInt(edtAguardar.Text) * 1000, StrToInt(edtAguardar.Text))
      else
        edtAguardar.Text := IntToStr(AguardarConsultaRet);

      if NaoEstaVazio(edtTentativas.Text) then
        Tentativas := StrToInt(edtTentativas.Text)
      else
        edtTentativas.Text := IntToStr(Tentativas);

      if NaoEstaVazio(edtIntervalo.Text) then
        IntervaloTentativas := ifThen(StrToInt(edtIntervalo.Text) < 1000,
          StrToInt(edtIntervalo.Text) * 1000, StrToInt(edtIntervalo.Text))
      else
        edtIntervalo.Text :=
          IntToStr(ACBreSocial1.Configuracoes.WebServices.IntervaloTentativas);

      TimeOut := seTimeOut.Value;
      ProxyHost := edtProxyHost.Text;
      ProxyPort := edtProxyPorta.Text;
      ProxyUser := edtProxyUser.Text;
      ProxyPass := edtProxySenha.Text;
    end;

    ACBreSocial1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

    cbxSalvarArqs.Checked := Ini.ReadBool('Arquivos', 'Salvar', False);
    cbxPastaMensal.Checked := Ini.ReadBool('Arquivos', 'PastaMensal', False);
    cbxAdicionaLiteral.Checked := Ini.ReadBool('Arquivos', 'AddLiteral', False);
    cbxEmissaoPatheSocial.Checked := Ini.ReadBool('Arquivos',
      'EmissaoPatheSocial', False);
    cbxSalvaPathEvento.Checked := Ini.ReadBool('Arquivos',
      'SalvarPathEvento', False);
    cbxSepararPorCNPJ.Checked := Ini.ReadBool('Arquivos',
      'SepararPorCNPJ', False);
    edtPatheSocial.Text := Ini.ReadString('Arquivos', 'PatheSocial', '');
    edtPathEvento.Text := Ini.ReadString('Arquivos', 'PathEvento', '');
    edtPathLogs.Text := Ini.ReadString('Geral', 'PathSalvar',
      PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
    edtPathSchemas.Text := Ini.ReadString('Geral', 'PathSchemas',
      PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Schemas\' +
      GetEnumName(TypeInfo(TVersaoeSocial), Integer(cbVersaoDF.ItemIndex)));

    with ACBreSocial1.Configuracoes.Arquivos do
    begin
      Salvar := cbxSalvarArqs.Checked;
      SepararPorMes := cbxPastaMensal.Checked;
      AdicionarLiteral := cbxAdicionaLiteral.Checked;
      EmissaoPatheSocial := cbxEmissaoPatheSocial.Checked;
      SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
      PathSalvar := edtPathLogs.Text;
      PathSchemas := edtPathSchemas.Text;
      PatheSocial := edtPatheSocial.Text;
    end;

    PathMensal := ACBreSocial1.Configuracoes.Arquivos.GetPatheSocial(0);

    ACBreSocial1.Configuracoes.Arquivos.PathSalvar := PathMensal;
  finally
    Ini.Free;
  end;
end;

procedure TFExemploEsocial.LimparDocsPasta;
var
  path: string;
  FileOp: TSHFileOpStruct;
begin
  try
    path := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Docs';
    FillChar(FileOp, SizeOf(FileOp), 0);
    FileOp.wFunc := FO_DELETE;
    FileOp.pFrom := PChar(path + #0); // double zero-terminated
    FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
    SHFileOperation(FileOp);
    ForceDirectories(path);
  except
  end;
end;

procedure TFExemploEsocial.SelecionaEventos;
begin
  ACBreSocial1.Eventos.Clear;

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
  if (cbS1250.Checked) then
    GerareSocial1250;
  if (cbS1260.Checked) then
    GerareSocial1260;
  if (cbS1270.Checked) then
    GerareSocial1270;
  if (cbS1280.Checked) then
    GerareSocial1280;
  if (cbS1295.Checked) then
    GerareSocial1295;
  if (cbS1298.Checked) then
    GerareSocial1298;
  if (cbS1299.Checked) then
    GerareSocial1299;
  if (cbS1300.Checked) then
    GerareSocial1300;
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
  if (cbS2306.Checked) then
    GerareSocial2306;
  if (cbS2399.Checked) then
    GerareSocial2399;
  if (cbS2400.Checked) then
    GerareSocial2400;
  if (cbS3000.Checked) then
    GerareSocial3000;
end;

procedure TFExemploEsocial.btnCarregarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Evento (Arquivo XML)';
  OpenDialog1.DefaultExt := '*.xml';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBreSocial1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
    ACBreSocial1.Eventos.LoadFromFile(OpenDialog1.FileName);

  MemoResp.Lines.Add('XML de Eventos Carregado com Sucesso!');
  pgWebService.ActivePageIndex := 3;
end;

procedure TFExemploEsocial.btnCarregarINIClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Evento (Arquivo INI)';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter := 'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBreSocial1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
    ACBreSocial1.Eventos.LoadFromINI(OpenDialog1.FileName);

  MemoResp.Lines.Add('INI de Eventos Carregado com Sucesso!');
  pgWebService.ActivePageIndex := 3;
end;

end.
