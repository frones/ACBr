{ ****************************************************************************** }
{ Projeto: Componente ACBreSocial }
{ Biblioteca multiplataforma de componentes Delphi para envio dos eventos do }
{ eSocial - http://www.esocial.gov.br/ }
{ }
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
  ACBrBase, ACBrDFe, ACBrUtil, ACBreSocial, ACBrMail,
  pcesConversaoeSocial, pcesS5001, pcesS5002, pcesS5011, pcesS5012,
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
    btnGerarEnviar: TButton;
    cbs2260: TCheckBox;

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
    procedure ACBreSocial1TransmissaoEventos(const AXML: AnsiString;
      ATipo: TeSocialEventos);
    procedure btnCarregarXMLClick(Sender: TObject);
    procedure btnCarregarINIClick(Sender: TObject);
    procedure btnGerarEnviarClick(Sender: TObject);
  private
    { Private declarations }
    function GetTipoOperacao: TModoLancamento;

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
    procedure GerareSocial2260;
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
begin
  with ACBreSocial1.Eventos.Iniciais.S1000.Add do
  begin
    with evtInfoEmpregador do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      // So tem na Versão 2.4.1
      // taProducao, taProducaoRestrita
      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoEmpregador do
      begin
        IdePeriodo.IniValid := '2015-05';
        IdePeriodo.FimValid := '2099-12';

        with InfoCadastro do
        begin
          if Checb_ZeraBase.Checked then
          begin
            NmRazao := 'RemoverEmpregadorDaBaseDeDadosDaProducaoRestrita';
            ClassTrib := ct00;
          end
          else
          begin
            NmRazao := 'Empresa Teste';
            ClassTrib := ct01;
          end;

          NatJurid := '0001';
          IndCoop := TpIndCoop(1);
          IndConstr := TpIndConstr(2);
          IndDesFolha := TpIndDesFolha(1);
          IndOptRegEletron := TpIndOptRegEletron(1);
          IndEtt := tpSimNao(1);
          nrRegEtt := '';

          with InfoOp do
          begin
            nrSiafi := '12345';

            infoEnte.nmEnte := 'Ente federativo teste';
            infoEnte.uf := tpuf(ufSP);
            infoEnte.vrSubteto := 100.00;
          end;

          with DadosIsencao do
          begin
            IdeMinLei := 'Sigla Min';
            NrCertif := '1111';
            DtEmisCertif := date;
            DtVencCertif := date;
            NrProtRenov := '10';
            DtProtRenov := date;
            DtDou := date;
            PagDou := '111';
          end;

          with Contato do
          begin
            NmCtt := 'Contato 1';
            CpfCtt := '00000222220';
            FoneFixo := '34335856';
            FoneCel := '991524587';
            email := 'testecontato@testecontato.com';
          end;

          InfoOrgInternacional.IndAcordoIsenMulta := tpIndAcordoIsencaoMulta(1);

          SoftwareHouse.Clear;

          with SoftwareHouse.Add do
          begin
            CnpjSoftHouse := '00000000000000';
            NmRazao := 'SoftwareHouse Teste';
            NmCont := 'Soft Contato';
            Telefone := '34335856';
            email := 'teste@teste.com';
          end;

          with InfoComplementares do
          begin
            SituacaoPJ.IndSitPJ := tpIndSitPJ(0);
            SituacaoPF.IndSitPF := tpIndSitPF(0);
          end;
        end;

        NovaValidade.IniValid := '2014-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1005;
begin
  with ACBreSocial1.Eventos.Iniciais.S1005.Add do
  begin
    with evtTabEstab do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with infoEstab do
      begin
        with IdeEstab do
        begin
          TpInsc := tiCNPJ;
          NrInsc := '012345678901234';
          IniValid := '2015-05';
          FimValid := '2099-12';
        end;

        with DadosEstab do
        begin
          cnaePrep := '2015';

          with aliqGilrat do
          begin
            AliqRat := arat1;
            Fap := 1.5;
            AliqRatAjust := 2.5;

            with ProcAdmJudRat do
            begin
              tpProc := tpTpProc(1);
              nrProc := '20150512';
              codSusp := '1';
              tpProc := tpTpProc(1);
              nrProc := '20150512';
              codSusp := '2';
            end;
          end;

          infoCaepf.tpCaepf := tcContrIndividual;

          infoObra.indSubstPatrObra := tpIndSubstPatronalObra(1);

          with infoTrab do
          begin
            regPt := tpRegPt(3);

            with infoApr do
            begin
              contApr := tpContApr(2);
              nrProcJud := '20150612';
              contEntEd := tpSim;

              infoEntEduc.Clear;

              with infoEntEduc.Add do
                NrInsc := '0123456789';
            end;

            infoPCD.contPCD := tpContPCD(9);
            infoPCD.nrProcJud := '20160131';
          end;
        end;

        NovaValidade.IniValid := '2014-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1010;
begin
  with ACBreSocial1.Eventos.Tabelas.S1010.Add do
  begin
    with EvtTabRubrica do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoRubrica do
      begin
        with ideRubrica do
        begin
          CodRubr := '5445';
          ideTabRubr := '100000';
          IniValid := '2015-05';
          FimValid := '2015-06';
        end;

        with DadosRubrica do
        begin
          dscRubr := 'Teste de S-1010';
          natRubr := 1022;
          tpRubr := tpTpRubr(1);
          codIncCP := tpCodIncCP(1);
          codIncIRRF := tpCodIncIRRF(1);
          codIncFGTS := tpCodIncFGTS(1);
          codIncSIND := tpCodIncSIND(1);
          observacao := 'Rubrica Teste';

          IdeProcessoCP.Clear;

          with IdeProcessoCP.Add do
          begin
            nrProc := '1020';
            ExtDecisao := tpExtDecisao(1);
            codSusp := '1';
          end;

          IdeProcessoIRRF.Clear;

          with IdeProcessoIRRF.Add do
          begin
            nrProc := '1020';
            codSusp := '2';
          end;

          IdeProcessoFGTS.Clear;

          with IdeProcessoFGTS.Add do
          begin
            nrProc := '50740';
          end;

          IdeProcessoSIND.Clear;

          with IdeProcessoSIND.Add do
          begin
            nrProc := '50';
          end;
        end;

        if (ModoLancamento = mlAlteracao) then
        begin
          NovaValidade.IniValid := '2015-05';
          NovaValidade.FimValid := '2099-12';
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1020;
begin
  with ACBreSocial1.Eventos.Tabelas.S1020.Add do
  begin
    with EvtTabLotacao do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with infoLotacao do
      begin
        IdeLotacao.codLotacao := '300000';
        IdeLotacao.IniValid := '2015-06';
        IdeLotacao.FimValid := '2099-12';

        with dadosLotacao do
        begin
          tpLotacao := '01';
          TpInsc := tiCAEPF;
          NrInsc := '6564646565';

          with fPasLotacao do
          begin
            Fpas := '515';
            codTercs := '0015';
            codTercsSusp := '0506';

            with infoProcJudTerceiros do
            begin
              procJudTerceiro.Clear;

              with procJudTerceiro.Add do
              begin
                codTerc := '1111';
                nrProcJud := '1234567891239-1345';
                codSusp := '1';
              end;
            end;
          end;

          with infoEmprParcial do
          begin
            tpInscContrat := tpTpInscContratante(0);
            NrInscContrat := '74563214500045';
            tpInscProp := TpTpInscProp(0);
            nrInscProp := '654234523416';
          end;
        end;

        NovaValidade.IniValid := '2015-06';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1030;
begin
  with ACBreSocial1.Eventos.Tabelas.S1030.Add do
  begin
    with EvtTabCargo do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoCargo do
      begin
        IdeCargo.CodCargo := '37';
        IdeCargo.IniValid := '2015-05';
        IdeCargo.FimValid := '2099-12';

        with DadosCargo do
        begin
          nmCargo := 'Programador';
          codCBO := '500000';

          with cargoPublico do
          begin
            acumCargo := tpAcumCargo(0);
            contagemEsp := tpContagemEsp(0);
            dedicExcl := tpSimNao(0);

            leiCargo.nrLei := '11111';
            leiCargo.dtLei := Now;
            leiCargo.sitCargo := tpSitCargo(0);
          end;
        end;

        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1035;
begin
  with ACBreSocial1.Eventos.Tabelas.S1035.Add do
  begin
    with evtTabCarreira do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoCarreira do
      begin
        ideCarreira.codCarreira := '1';
        ideCarreira.IniValid := '2015-05';
        ideCarreira.IniValid := '2099-12';

        dadosCarreira.dscCarreira := 'Juiz';
        dadosCarreira.leiCarr := 'lei89489/77';
        dadosCarreira.dtLeiCarr := Now;
        dadosCarreira.sitCarr := tpSitCarr(0);

        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1040;
begin
  with ACBreSocial1.Eventos.Tabelas.S1040.Add do
  begin
    with EvtTabFuncao do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoFuncao do
      begin
        IdeFuncao.CodFuncao := '1';
        IdeFuncao.IniValid := '2015-05';
        IdeFuncao.FimValid := '2099-12';

        DadosFuncao.dscFuncao := 'PROGRAMADOR';
        DadosFuncao.codCBO := '5000';

        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1050;
begin
  with ACBreSocial1.Eventos.Tabelas.S1050.Add do
  begin
    with EvtTabHorContratual do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoHorContratual do
      begin
        ideHorContratual.codHorContrat := '1';
        ideHorContratual.IniValid := '2015-05';
        ideHorContratual.FimValid := '2099-12';

        with dadosHorContratual do
        begin
          hrEntr := '0800';
          hrSaida := '1800';
          durJornada := 525;
          perHorFlexivel := tpSimNao(1);

          horarioIntervalo.Clear;

          with horarioIntervalo.Add do
          begin
            tpInterv := tptpIntervalo(0);
            durInterv := 90;
            iniInterv := '1200';
            termInterv := '1330';
          end;

          with horarioIntervalo.Add do
          begin
            tpInterv := tptpIntervalo(1);
            durInterv := 15;
            iniInterv := '1645';
            termInterv := '1700';
          end;
        end;

        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1060;
begin
  with ACBreSocial1.Eventos.Tabelas.S1060.Add do
  begin
    with EvtTabAmbiente do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with infoAmbiente do
      begin
        ideAmbiente.codAmb := '123456';
        ideAmbiente.IniValid := '2015-05';
        ideAmbiente.FimValid := '2099-12';

        with dadosAmbiente do
        begin
          dscAmb := 'DESCRICAO';
          localAmb := tpLocalAmb(0);
          TpInsc := tpTpInscAmbTab(0);
          NrInsc := '123456789';

          fatorRisco.Clear;

          with fatorRisco.Add do
            codFatRis := '1111';

          with fatorRisco.Add do
            codFatRis := '2222';

          with fatorRisco.Add do
            codFatRis := '3333';
        end;

        NovaValidade.IniValid := '2015-06';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1070;
begin
  with ACBreSocial1.Eventos.Tabelas.S1070.Add do
  begin
    with EvtTabProcesso do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoProcesso do
      begin
        with ideProcesso do
        begin
          tpProc := tpTpProc(0);
          nrProc := '5000';
          IniValid := '2014-05';
          FimValid := '2015-06';
        end;

        with dadosProc do
        begin
          IndAutoria := tpindAutoria(0);
          indMatProc := tpIndMatProc(0);

          DadosProcJud.UfVara := 'PR';
          DadosProcJud.codMunic := 5075;
          DadosProcJud.IdVara := '20';

          infoSusp.Clear;

          with infoSusp.Add do
          begin
            codSusp := '1';
            indSusp := tpIndSusp(0);
            dtDecisao := Now;
            indDeposito := tpNao;
          end;
        end;

        NovaValidade.IniValid := '2015-10';
        NovaValidade.FimValid := '2016-10';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1080;
begin
  with ACBreSocial1.Eventos.Tabelas.S1080.Add do
  begin
    with EvtTabOperPortuario do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoOperPortuario do
      begin
        IdeOperPortuario.cnpjOpPortuario := '29813268000156';
        IdeOperPortuario.IniValid := '2015-05';
        IdeOperPortuario.FimValid := '2099-12';

        DadosOperPortuario.AliqRat := arat1;
        DadosOperPortuario.Fap := 0.5;
        DadosOperPortuario.AliqRatAjust := 5.5;

        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1200;
begin
  with ACBreSocial1.Eventos.Periodicos.S1200.Add do
  begin
    with evtRemun do
    begin
      Sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo  := '4564654'; Numero do recibo que será retificado.
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '052015';
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '012345678987654';

      with ideTrabalhador do
      begin
        CpfTrab := '01234567890';
        NisTrab := '09876543210';

        with infoMV do
        begin
          indMV := imvDescontadaempregador;

          { Os Grupos abaixo são opcionais
            O grupo abaixocorresponde a funcionários que tenham dois empregos em empresas diferentes }
            remunOutrEmpr.Clear;

          with remunOutrEmpr.Add do
          begin
            TpInsc := tiCNPJ;
            NrInsc := '01234567890123';
            CodCateg := 222;
            vlrRemunOE := 1230.10;
          end;
        end;

        // o grupo abaixo corresponde apenas a trabalhadores cuja categoria não está sujeita ao evento de admissão
        // ou TSV-início
        with infoComplem do
        begin
          NmTrab := 'João das Neves';
          DtNascto := date;
          codCBO := '000001';
          NatAtividade := navUrbano;
          qtdDiasTrab := 10;

          with sucessaoVinc do
          begin
            cnpjEmpregAnt := '12345678987654';
            MatricAnt := '123';
            DtAdm := Now;
            observacao := 'obs sucessao vinc';
          end;
        end;

        // os dados abaixo só devem ser informados em caso do processo existir e houver decisão que incida sobre as
        // contribuições
        procJudTrab.Clear;

        with procJudTrab.Add do
        begin
          tpTrib := tptPrevidenciaria;
          nrProcJud := '95135703320156150258';
          codSusp := 1;
        end;
      end;

      dmDev.Clear;

      with dmDev.Add do
      begin
        ideDmDev := '1';
        CodCateg := 111;

        with infoPerApur.ideEstabLot.Add do
        begin
          TpInsc := tiCNPJ;
          NrInsc := '012345678987654';
          codLotacao := 'SACI54321';
          qtdDiasAv := 22;

          remunPerAnt.Clear;

          with remunPerApur.Add do
          begin
            Matricula := 'A1234';
            indSimples := idsIntegralmente;

            itensRemun.Clear;

            with itensRemun.Add do
            begin
              CodRubr := '987654';
              ideTabRubr := 'E380';
              qtdRubr := 100;
              fatorRubr := 50;
              vrUnit := 3296.35;
              vrRubr := 3330.30;
            end;

            infoSaudeColet.detOper.Clear;

            with infoSaudeColet.detOper.Add do
            begin
              cnpjOper := '01234567898765';
              regANS := 'A1B2C3';
              vrPgTit := 1.50;

              detPlano.Clear;

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

        infoPerAnt.ideADC.Clear;

        with infoPerAnt.ideADC.Add do
        begin
          dtAcConv := Now;
          tpAcConv := tacLegislacaoFederalEstadualMunicipalDistrital;
          dtEfAcConv := Now;
          compAcConv := '2017-01';
          dsc := 'Dissídio';

          idePeriodo.Clear;

          with IdePeriodo.Add do
          begin
            perRef := '201504';

            ideEstabLot.Clear;

            with ideEstabLot.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '01234567898765';
              codLotacao := 'TESTE123';

              remunPerAnt.Clear;

              with remunPerAnt.Add do
              begin
                Matricula := 'A1234';
                indSimples := idsIntegralmente;

                itensRemun.Clear;

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

        infoTrabInterm.Clear;

        with infoTrabInterm.Add do
          codConv := '123456';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1202;
begin
  with ACBreSocial1.Eventos.Periodicos.S1202.Add do
  begin
    with evtRmnRPPS do
    begin
      Sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo  := '4564654'; Numero do recibo que será retificado.
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '052015';
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '012345678987654';

      with ideTrabalhador do
      begin
        CpfTrab := '01234567890';
        NisTrab := '09876543210';
        qtdDepFP := 0;

        // os dados abaixo só devem ser informados em caso do processo existir
        // e houver decisão que incida sobre as  contribuições
        procJudTrab.Clear;

        with procJudTrab.Add do
        begin
          tpTrib := tptPrevidenciaria;
          nrProcJud := '95135703320156150258';
          codSusp := 1;
        end;
      end;

      dmDev.Clear;

      with dmDev.Add do
      begin
        ideDmDev := '1';

        infoPerApur.ideEstab.Clear;

        with infoPerApur.IdeEstab.Add do
        begin
          TpInsc := tiCNPJ;
          NrInsc := '012345678987654';

          remunPerApur.Clear;

          with remunPerApur.Add do
          begin
            Matricula := 'A1234';
            CodCateg := 101;

            itensRemun.Clear;

            with itensRemun.Add do
            begin
              CodRubr := '987654';
              ideTabRubr := 'E380';
              qtdRubr := 100;
              fatorRubr := 50;
              vrUnit := 3296.35;
              vrRubr := 3330.30;
            end;

            infoSaudeColet.detOper.Clear;

            with infoSaudeColet.detOper.Add do
            begin
              cnpjOper := '01234567898765';
              regANS := 'A1B2C3';
              vrPgTit := 1.50;

              detPlano.Clear;

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

        infoPerAnt.ideADC.Clear;

        with infoPerAnt.ideADC.Add do
        begin
          dtLei := Now;
          nrLei := '321321/2017';
          dtEf := Now;

          idePeriodo.Clear;

          with IdePeriodo.Add do
          begin
            perRef := '2015-03';

            ideEstab.Clear;

            with IdeEstab.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '01234567898765';

              remunPerAnt.Clear;

              with remunPerAnt.Add do
              begin
                Matricula := 'A1234';
                CodCateg := 101;

                itensRemun.Clear;

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
end;

procedure TFExemploEsocial.GerareSocial1207;
begin
  with ACBreSocial1.Eventos.Periodicos.S1207.Add do
  begin
    with evtBenPrRP do
    begin
      Sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2017-05';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      ideBenef.cpfBenef := '88888888888';

      dmDev.Clear;

      with dmDev.Add do
      begin
        tpBenef := 01;
        nrBenefic := '3132132';
        ideDmDev := '1';

        itens.Clear;

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
end;

procedure TFExemploEsocial.GerareSocial1210;
begin
  with ACBreSocial1.Eventos.Periodicos.S1210.Add do
  begin
    with evtPgtos do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; - obrigatório se indRetif = ireRetificacao.
        IndApuracao := iapuMensal;
        perApur := '052015';
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '9632587410123';

      with IdeBenef do
      begin
        cpfBenef := '01478523690';
        deps.vrDedDep := 100.50;

        InfoPgto.Clear;

        with InfoPgto.Add do
        begin
          DtPgto := StrToDate('10/06/2015');
          tpPgto := tpPgtoRemun1200;
          IndResBr := tpNao;

          // -OS GRUPOS ABAIXO SÃO OPCIONAIS
          // grupo detPgtoFl agora é um collection
          detPgtoFl.Clear;

          with detPgtoFl.Add do
          begin
            perRef := '052015';
            ideDmDev := '2';
            indPagtoTt := tpSim;
            vrLiq := 12365.43;
            nrRecArq := '132156156';

            retPagtoTot.Clear;

            with retPagtoTot.Add do
            begin
              CodRubr := '1';
              ideTabRubr := '0';
              qtdRubr := 1.5;
              fatorRubr := 50;
              vrUnit := 100.10;
              vrRubr := 1001.00;

              penAlim.Clear;

              with penAlim.Add do
              begin
                cpfBenef := '12345698745';
                dtNasctoBenef := Now;
                nmBenefic := 'Beneficiário da pensão';
                vlrPensao := 556.32;
              end;
            end;

            infoPgtoParc.Clear;

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

          with detPgtoBenPr do
          begin
            perRef := '2017-01';
            ideDmDev := '1';
            indPgtoTt := tpNao;
            vrLiq := 1500.21;

            retPgtoTot.Clear;

            with retPgtoTot.Add do
            begin
              CodRubr := '321';
              ideTabRubr := '0';
              qtdRubr := 1.5;
              fatorRubr := 50.65;
              vrUnit := 500.85;
              vrRubr := 5001.65;
            end;

            infoPgtoParc.Clear;

            with infoPgtoParc.Add do
            begin
              CodRubr := '555';
              ideTabRubr := '0';
              qtdRubr := 2;
              fatorRubr := 40.11;
              vrUnit := 842.85;
              vrRubr := 774.65;
            end;
          end;

          detPgtoFer.Clear;

          with detPgtoFer.Add do
          begin
            CodCateg := 111;
            dtIniGoz := Now;
            qtDias := 30;
            vrLiq := 2500.32;

            detRubrFer.Clear;

            with detRubrFer.Add do
            begin
              CodRubr := '888';
              ideTabRubr := '0';
              qtdRubr := 1;
              fatorRubr := 100;
              vrUnit := 144.33;
              vrRubr := 2500.32;

              penAlim.Clear;

              with penAlim.Add do
              begin
                cpfBenef := '44455588899';
                dtNasctoBenef := Now;
                nmBenefic := 'Beneficiário de Pensão nas Férias';
                vlrPensao := 250.32;
              end;
            end;
          end;

          detPgtoAnt.Clear;

          with detPgtoAnt.Add do
          begin
            CodCateg := 111;

            infoPgtoAnt.Clear;

            with infoPgtoAnt.Add do
            begin
              tpBcIRRF := tpCodIncIRRF(0);
              vrBcIRRF := 2500.32;
            end;
          end;

          with IdePgtoExt do
          begin
            idePais.codPais := '116';
            idePais.indNIF := infBeneficiaNIF;
            idePais.nifBenef := 'ABCDEFGH123456789';

            with endExt do
            begin
              DscLograd := 'Abbey Road St';
              NrLograd := '93';
              complem := 'apto 11';
              Bairro := 'Sgt Peppers';
              NmCid := 'Liverpool';
              CodPostal := '9999999999';
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1250;
begin
  with ACBreSocial1.Eventos.Periodicos.S1250.Add do
  begin
    with EvtAqProd do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoAquisProd.IdeEstabAdquir do
      begin
        tpInscAdq := tiCNPJ;
        nrInscAdq := '12345678910001';

        TpAquis.Clear;

        with TpAquis.Add do
        begin
          indAquis := tpIdAquis(0);
          vlrTotAquis := 520000.80;

          IdeProdutor.Clear;

          with IdeProdutor.Add do
          begin
            tpInscProd := tiCNPJ;
            nrInscProd := '98765432100015';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.Add do
            begin
              serie := '00004';
              nrDocto := '64896549898789';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

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

            InfoProcJud.Clear;

            with InfoProcJud.Add do
            begin
              nrProcJud := '56464897';
              codSusp := 333;
              vrCPNRet := 99999.99;
              vrRatNRet := 88888.88;
              vrSenarNRet := 77777.77;
            end;
          end;

          with IdeProdutor.Add do
          begin
            tpInscProd := tiCNPJ;
            nrInscProd := '98765432100015';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

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

            InfoProcJud.Clear;

            with InfoProcJud.Add do
            begin
              nrProcJud := '56464897';
              codSusp := 222;
              vrCPNRet := 99999.99;
              vrRatNRet := 88888.88;
              vrSenarNRet := 77777.77;
            end;
          end;

        end;

        with TpAquis.Add do
        begin
          indAquis := tpIdAquis(1);
          vlrTotAquis := 520000.80;

          IdeProdutor.Clear;

          with IdeProdutor.Add do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '74913476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.Add do
            begin
              serie := '00004';
              nrDocto := '64896549898789';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

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

            InfoProcJud.Clear;

            with InfoProcJud.Add do
            begin
              nrProcJud := '00000002';
              codSusp := 222;
              vrCPNRet := 22222.22;
              vrRatNRet := 22222.22;
              vrSenarNRet := 22222.22;
            end;
          end;

          with IdeProdutor.Add do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '00003476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            InfoProcJud.Clear;

            with InfoProcJud.Add do
            begin
              nrProcJud := '33333333';
              codSusp := 333;
              vrCPNRet := 33333.99;
              vrRatNRet := 33333.88;
              vrSenarNRet := 33333.77;
            end;
          end;
        end;

        with TpAquis.Add do
        begin
          indAquis := tpIdAquis(2);
          vlrTotAquis := 33300.80;

          IdeProdutor.Clear;

          with IdeProdutor.Add do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '74913476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.Add do
            begin
              serie := '00004';
              nrDocto := '64896549898789';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

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

            InfoProcJud.Clear;

            with InfoProcJud.Add do
            begin
              nrProcJud := '00000002';
              codSusp := 222;
              vrCPNRet := 22222.22;
              vrRatNRet := 22222.22;
              vrSenarNRet := 22222.22;
            end;
          end;

          with IdeProdutor.Add do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '00003476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            InfoProcJud.Clear;

            with InfoProcJud.Add do
            begin
              nrProcJud := '33333333';
              codSusp := 333;
              vrCPNRet := 33333.99;
              vrRatNRet := 33333.88;
              vrSenarNRet := 33333.77;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1260;
begin
  with ACBreSocial1.Eventos.Periodicos.S1260.Add do
  begin
    with EvtComProd do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCPF;
      IdeEmpregador.NrInsc := '0123456789';

      with InfoComProd do
      begin
        with IdeEstabel do
        begin
          nrInscEstabRural := '123456789';

          TpComerc.Clear;

          with TpComerc.Add do
          begin
            indComerc := tpIndComerc(0);
            vrTotCom := 5000.80;

            IdeAdquir.Clear;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;

              nfs.Clear;

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

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;

              nfs.Clear;

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

          with TpComerc.Add do
          begin
            indComerc := tpIndComerc(1);
            vrTotCom := 5000.80;

            IdeAdquir.Clear;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
            end;

            with IdeAdquir.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;

              nfs.Clear;

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

            InfoProcJud.Clear;

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
  end;
end;

procedure TFExemploEsocial.GerareSocial1270;
begin
  with ACBreSocial1.Eventos.Periodicos.S1270.Add do
  begin
    with EvtContratAvNP do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '0123456789';

      remunAvNp.Clear;

      with RemunAvNP.Add do
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

      with RemunAvNP.Add do
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

      with RemunAvNP.Add do
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

      with RemunAvNP.Add do
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
end;

procedure TFExemploEsocial.GerareSocial1280;
begin
  with ACBreSocial1.Eventos.Periodicos.S1280.Add do
  begin
    with EvtInfoComplPer do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      InfoSubstPatr.indSubstPatr := tpIndSubstPatrOpPort(0);
      InfoSubstPatr.percRedContrib := 500.20;

      InfoSubstPatrOpPort.Clear;

      with InfoSubstPatrOpPort.Add do
      begin
        cnpjOpPortuario := '12345678900112';
      end;

      with InfoSubstPatrOpPort.Add do
      begin
        cnpjOpPortuario := '98765432100014';
      end;

      InfoAtivConcom.fatorMes := 9.00;
      InfoAtivConcom.fator13 := 1.00;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1295;
begin
  with ACBreSocial1.Eventos.Periodicos.S1295.Add do
  begin
    with evtTotConting do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      with IdeRespInf do
      begin
        nmResp := 'Responsavel teste';
        cpfResp := '12345678950';
        Telefone := '46 - 22222222';
        email := 'Responsavelteste@email.com';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1298;
begin
  with ACBreSocial1.Eventos.Periodicos.S1298.Add do
  begin
    with EvtReabreEvPer do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1299;
begin
  with ACBreSocial1.Eventos.Periodicos.S1299.Add do
  begin
    with EvtFechaEvPer do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      with IdeRespInf do
      begin
        nmResp := 'Responsavel teste';
        cpfResp := '12345678950';
        Telefone := '46 - 22222222';
        email := 'Responsavelteste@email.com';
      end;

      with InfoFech do
      begin
        evtRemun := tpSimNao(0);
        EvtPgtos := tpSimNao(1);
        EvtAqProd := tpSimNao(1);
        EvtComProd := tpSimNao(0);
        EvtContratAvNP := tpSimNao(1);
        EvtInfoComplPer := tpSimNao(0);
        compSemMovto := '07-2015';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial1300;
begin
  with ACBreSocial1.Eventos.Periodicos.S1300.Add do
  begin
    with EvtContrSindPatr do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := tpIndApuracao(iapuMensal);
        perApur := '2015-06';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      ContribSind.Clear;

      with ContribSind.Add do
      begin
        cnpjSindic := '01234567891111';
        tpContribSind := tpTpContribSind(0);
        vlrContribSind := 1500.50;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2190;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2190.Add do
  begin
    with EvtAdmPrelim do
    begin
      Sequencial := 0;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '12345678901234';

      InfoRegPrelim.CpfTrab := '12345678901';
      InfoRegPrelim.DtNascto := Now - 9125;
      InfoRegPrelim.DtAdm := Now;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2200;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2200.Add do
  begin
    with EvtAdmissao do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        // indRetif := tpIndRetificacao(1);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678901234';

      with Trabalhador do
      begin
        CpfTrab := '54564654564';
        NisTrab := '12345678901';
        NmTrab := 'Empregado teste';
        Sexo := 'M';
        RacaCor := 1;
        EstCiv := 1;
        GrauInstr := '10';
        IndPriEmpr := tpNao;
        nmSoc := 'Nome social';

        with Nascimento do
        begin
          DtNascto := date;
          codMunic := 51268;
          uf := 'PR';
          PaisNascto := '565';
          PaisNac := '545';
          NmMae := 'teste mae';
          NmPai := 'teste pai';
        end;

        with Documentos do
        begin
          CTPS.NrCtps := '56454';
          CTPS.SerieCtps := '646';
          CTPS.UfCtps := 'PR';

          RIC.NrRic := '123123';
          RIC.OrgaoEmissor := 'SSP';
          RIC.DtExped := date;

          RG.NrRg := '123123';
          RG.OrgaoEmissor := 'SSP';
          RG.DtExped := date;

          RNE.NrRne := '123123';
          RNE.OrgaoEmissor := 'SSP';
          RNE.DtExped := date;

          OC.NrOc := '999';
          OC.OrgaoEmissor := 'SSP';
          OC.DtExped := date;
          OC.DtValid := date;

          CNH.nrRegCnh := '999';
          CNH.DtExped := date;
          CNH.ufCnh := tpuf(ufPR);
          CNH.DtValid := date;
          CNH.dtPriHab := date;
          CNH.categoriaCnh := tpCnh(cnA);
        end;

        with Endereco do
        begin
          with Brasil do
          begin
            TpLograd := 'RUA';
            DscLograd := 'TESTE';
            NrLograd := '777';
            Complemento := 'AP 101';
            Bairro := 'CENTRO';
            Cep := '85500000';
            codMunic := 11111;
            uf := tpuf(ufPR);
          end;

          with Exterior do
          begin
            PaisResid := '545';
            DscLograd := 'TESTE';
            NrLograd := '777';
            Complemento := 'AP 101';
            Bairro := 'CENTRO';
            NmCid := 'CIDADE EXTERIOR';
            CodPostal := '50000';
          end;
        end;

        with TrabEstrangeiro do
        begin
          DtChegada := date;
          ClassTrabEstrang := tpClassTrabEstrang(ctVistoPermanente);
          CasadoBr := 'N';
          FilhosBr := 'N';
        end;

        with InfoDeficiencia do
        begin
          DefFisica := tpNao;
          DefVisual := tpNao;
          DefAuditiva := tpNao;
          DefMental := tpNao;
          DefIntelectual := tpNao;
          ReabReadap := tpSim;
          infoCota := tpNao;
          observacao := 'sem deficiencia';
        end;

        Dependente.Clear;

        with Dependente.Add do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
        end;

        with Dependente.Add do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
        end;

        Aposentadoria.TrabAposent := tpNao;

        with Contato do
        begin
          FonePrinc := '91067240';
          FoneAlternat := '91067240';
          EmailPrinc := 'TESTE@email.com.br';
          EmailAlternat := 'teste@teste.com';
        end;
      end;

      with Vinculo do
      begin
        Matricula := '54545';
        TpRegTrab := tpTpRegTrab(1);
        TpRegPrev := tpTpRegPrev(1);
        NrRecInfPrelim := '9999999999';

        with InfoRegimeTrab do
        begin
          with InfoCeletista do
          begin
            DtAdm := date;
            TpAdmissao := tpTpAdmissao(1);
            IndAdmissao := tpTpIndAdmissao(iaNormal);
            TpRegJor := tpTpRegJor(1);
            NatAtividade := tpNatAtividade(navUrbano);
            dtBase := 03;
            cnpjSindCategProf := '12345678901234';

            FGTS.OpcFGTS := tpOpcFGTS(ofOptante);
            FGTS.DtOpcFGTS := date;

            with TrabTemporario do
            begin
              hipLeg := 1;
              justContr := 'teste';
              tpinclContr := icLocaisSemFiliais;

              with IdeTomadorServ do
              begin
                TpInsc := tiCNPJ;
                NrInsc := '12345678901234';
                ideEstabVinc.TpInsc := tiCNPJ;
                ideEstabVinc.NrInsc := '12345678901234';
              end;

              IdeTrabSubstituido.Clear;

              with IdeTrabSubstituido.Add do
                CpfTrabSubst := '12345678912';
            end;

            aprend.TpInsc := tpTpInsc(1);
            aprend.NrInsc := '98765432109';
          end;

          // enviar apenas um tipo de admissao
          (*
          with InfoEstatutario do
          begin
            IndProvim   := tpIndProvim(ipNormal);
            TpProv      := tpTpProv(tpNomeacaoCargoEfetivo);
            DtNomeacao  := Date;
            DtPosse     := Date;
            DtExercicio := Date;
          end;
          *)
        end;

        with InfoContrato do
        begin
          CodCargo := '545';
          CodFuncao := '5456';
          CodCateg := 111;
          codCarreira := '1';
          dtIngrCarr := Now;

          Remuneracao.VrSalFx := 5000;
          Remuneracao.UndSalFixo := tpUndSalFixo(5);
          Remuneracao.DscSalVar := 'nada a declarar';

          Duracao.TpContr := tpTpContr(1);
          Duracao.dtTerm := date;

          with LocalTrabalho do
          begin
            LocalTrabGeral.TpInsc := tiCNPJ;
            LocalTrabGeral.NrInsc := '21354632';
            LocalTrabGeral.DescComp := 'Descricao local geral teste';

            with LocalTrabDom do
            begin
              TpLograd    := '123';
              DscLograd   := 'LOCAL DOMESTICO';
              NrLograd    := '111';
              Complemento := 'Complemento';
              Bairro      := 'Bairro';
              Cep         := '85202630';
              CodMunic    := 123;
              Uf          := tpuf(ufPR);
            end;
          end;

          with HorContratual do
          begin
            QtdHrsSem := 44;
            TpJornada := tpTpJornada(1);
            DscTpJorn := 'horario contratual';
            tmpParc := tpNaoeTempoParcial;

            horario.Clear;

            with horario.Add do
            begin
              Dia := tpTpDia(diSegundaFeira);
              codHorContrat := '54';
            end;

            with horario.Add do
            begin
              Dia := tpTpDia(diTercaFeira);
              codHorContrat := '10';
            end;
          end;

          FiliacaoSindical.Clear;

          with FiliacaoSindical.Add do
            CnpjSindTrab := '12345678901234';

          AlvaraJudicial.nrProcJud := '123';

          observacoes.Clear;

          with observacoes.Add do
            observacao := 'Observacao';
        end;

        with SucessaoVinc do
        begin
          cnpjEmpregAnt := '12345678901234';
          MatricAnt := '123';
          dtTransf := date;
          observacao := 'transferido';
        end;

        transfDom.cpfSubstituido := '12345678901';
        transfDom.MatricAnt := '123';
        transfDom.dtTransf := date;

        Afastamento.DtIniAfast := Now;
        Afastamento.codMotAfast := mtvAcidenteDoencaTrabalho;

        Desligamento.DtDeslig := Now;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2205;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2205.Add do
  begin
    with EvtAltCadastral do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(1);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '12345678901234';

      ideTrabalhador.CpfTrab := '12345678901';

      dtAlteracao := Now;

      with Trabalhador do
      begin
        NisTrab := '12345678901';
        NmTrab := 'Empregado teste';
        Sexo := 'M';
        RacaCor := 1;
        EstCiv := 1;
        GrauInstr := '10';
        nmSoc := 'Nome social';

        with Documentos do
        begin
          CTPS.NrCtps := '56454';
          CTPS.SerieCtps := '646';
          CTPS.UfCtps := 'PR';

          RIC.NrRic := '123123';
          RIC.OrgaoEmissor := 'SSP';
          RIC.DtExped := date;

          RG.NrRg := '123123';
          RG.OrgaoEmissor := 'SSP';
          RG.DtExped := date;

          RNE.NrRne := '123123';
          RNE.OrgaoEmissor := 'SSP';
          RNE.DtExped := date;

          OC.NrOc := '999';
          OC.OrgaoEmissor := 'SSP';
          OC.DtExped := date;
          OC.DtValid := date;

          CNH.nrRegCnh := '999';
          CNH.DtExped := date;
          CNH.ufCnh := tpuf(ufPR);
          CNH.DtValid := date;
          CNH.dtPriHab := date;
          CNH.categoriaCnh := tpCnh(cnA);
        end;

        with Endereco do
        begin
          with Brasil do
          begin
            TpLograd := 'RUA';
            DscLograd := 'TESTE';
            NrLograd := '777';
            Complemento := 'AP 101';
            Bairro := 'CENTRO';
            Cep := '85500000';
            codMunic := 11111;
            uf := tpuf(ufPR);
          end;

          with Exterior do
          begin
            PaisResid := '545';
            DscLograd := 'TESTE';
            NrLograd := '777';
            Complemento := 'AP 101';
            Bairro := 'CENTRO';
            NmCid := 'CIDADE EXTERIOR';
            CodPostal := '50000';
          end;
        end;

        with TrabEstrangeiro do
        begin
          DtChegada := date;
          ClassTrabEstrang := tpClassTrabEstrang(ctVistoPermanente);
          CasadoBr := 'N';
          FilhosBr := 'N';
        end;

        with InfoDeficiencia do
        begin
          DefFisica := tpNao;
          DefVisual := tpNao;
          DefAuditiva := tpNao;
          DefMental := tpNao;
          DefIntelectual := tpNao;
          ReabReadap := tpSim;
          infoCota := tpNao;
          observacao := 'sem deficiencia';
        end;

        Dependente.Clear;

        with Dependente.Add do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
        end;

        with Dependente.Add do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao
        end;

        Aposentadoria.TrabAposent := tpNao;

        with Contato do
        begin
          FonePrinc := '91067240';
          FoneAlternat := '91067240';
          EmailPrinc := 'TESTE@email.com.br';
          EmailAlternat := 'teste@teste.com';
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2206;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2206.Add do
  begin
    with EvtAltContratual do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif = ireRetificacao;
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '96325874103';
      IdeVinculo.Matricula := 'A1234';

      with AltContratual do
      begin
        dtAlteracao := date;
        dtEf := Now;
        dscAlt := 'descrição da alteração';

        Vinculo.TpRegTrab := trCLT;
        Vinculo.TpRegPrev := rpRGPS;

        with infoRegimeTrab do
        begin
          with InfoCeletista do
          begin
            TpRegJor := rjSubmetidosHorarioTrabalho;
            NatAtividade := navUrbano;
            dtBase := 08;
            cnpjSindCategProf := '15975395135700';

            trabTemporario.justProrr := 'Prorrogado porque eu quis';
          end;

          // InfoEstatutario.tpPlanRP := tpPlanRP(0);
        end;

        with infoContrato do
        begin
          CodCargo := '123';
          CodFuncao := '321';
          CodCateg := 111;
          codCarreira := '1';
          dtIngrCarr := Now;

          Remuneracao.VrSalFx := 780.00;
          Remuneracao.UndSalFixo := sfPorMes;
          Remuneracao.DscSalVar := 'Descrição de salário variável, obrigatório caso UndSalFixo for sfNaoAplicavel';

          Duracao.TpContr := PrazoIndeterminado;
          // Duracao.dtTerm  := Date; // Obrigatório se TpContr = PrazoDeterminado!

          // LocalTrabGeral não deve ser preenchido no caso de trabalhador doméstico.
          with LocalTrabalho do
          begin
            LocalTrabGeral.TpInsc := tiCNPJ;
            LocalTrabGeral.NrInsc := '12345678901234';
            LocalTrabGeral.DescComp := 'Descrição complementar do local de trabalho.';

            // LocalTrabDom - exclusivo para trabalhador doméstico,
            // indicando endereço onde exerce suas atividades
            (*
            with LocalTrabDom do
            begin
              TpLograd    := '001';
              DscLograd   := 'Rua das Hortencias';
              NrLograd    := '12';
              Complemento := 'Fundos';
              Bairro      := 'Jardim das Flores';
              Cep         := '11001001';
              CodMunic    := 1234567;
              Uf          := ufPr;
            end;
            *)
          end;

          with HorContratual do
          begin
            QtdHrsSem := 44;
            TpJornada := tjDemaisTiposJornada;
            DscTpJorn := 'Descrição do tipo de jornada, obrigatório se tpJornada = tjDemaisTiposJornada';
            tmpParc := tpNaoeTempoParcial;

            horario.Clear;

            with horario.Add do
            begin
              Dia := diSegundaFeira;
              codHorContrat := '001';
            end;
          end;

          FiliacaoSindical.Clear;

          with FiliacaoSindical.Add do
            CnpjSindTrab := '12345678901234';

          AlvaraJudicial.nrProcJud := '543216';

          servPubl.mtvAlter := maPromocao;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2210;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2210.Add do
  begin
    with EvtCAT do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeRegistrador.tpRegistrador := tpTpRegistrador(0);
      IdeRegistrador.TpInsc := tpTpInsc(1);
      IdeRegistrador.NrInsc := '12345678901234';

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '12345678901234';

      ideTrabalhador.CpfTrab := '12345678901';
      ideTrabalhador.NisTrab := '12345678901';

      with Cat do
      begin
        dtAcid := Now;
        TpAcid := '1.0.01';
        hrAcid := '1200';
        hrsTrabAntesAcid := '0400';
        tpCat := tpTpCat(0);
        indCatObito := tpNao;
        dtOBito := Now;
        indComunPolicia := tpSim;
        codSitGeradora := 200004300;
        iniciatCAT := tpIniciatCAT(1);
        observacao := 'Teste';

        with LocalAcidente do
        begin
          tpLocal := tpTpLocal(1);
          dscLocal := 'Local Teste';
          DscLograd := 'Logradouro Teste';
          NrLograd := '111';
          codMunic := 123;
          uf := tpuf(ufPR);
          cnpjLocalAcid := '12345678901234';
          pais := '008';
          CodPostal := '6546';
        end;

        ParteAtingida.Clear;

        with ParteAtingida.Add do
        begin
          codParteAting := 753030000;
          lateralidade := tpLateralidade(1);
        end;

        with ParteAtingida.Add do
        begin
          codParteAting := 753070700;
          lateralidade := tpLateralidade(2);
        end;

        with ParteAtingida.Add do
        begin
          codParteAting := 753510200;
          lateralidade := tpLateralidade(3);
        end;

        AgenteCausador.Clear;

        with AgenteCausador.Add do
          codAgntCausador := 302010300;

        with AgenteCausador.Add do
          codAgntCausador := 302010600;

        with AgenteCausador.Add do
          codAgntCausador := 302050500;

        with Atestado do
        begin
          codCNES := '1234567';
          dtAtendimento := Now;
          hrAtendimento := '1330';
          indInternacao := tpSim;
          durTrat := 5;
          indAfast := tpSim;
          dscLesao := 1;
          dscCompLesao := 'Descricao complementar';
          diagProvavel := 'Diagnostico teste';
          codCID := '1234';
          observacao := 'Observação teste';

          with Emitente do
          begin
            nmEmit := 'Emitente Teste';
            ideOC := tpIdeOC(1);
            NrOc := '456123';
            ufOC := tpuf(ufPR);
          end;
        end;

        CatOrigem.dtCatOrig := Now;
        CatOrigem.nrCatOrig := '123456';
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2220;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2220.Add do
  begin
    with evtMonit do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := TpTpAmb(1);
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '5000';

      with Aso do
      begin
        DtAso := date;
        tpAso := tpTpAso(1);
        ResAso := tpResAso(1);

        Exame.Clear;

        with Exame.Add do
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

        with Exame.Add do
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

        with IdeServSaude do
        begin
          codCNES := '9876541';
          FrmCtt := 'Telefone: 32200000';
          email := 'teste@teste.com';

          Medico.NmMed := 'MEDICO TESTE';
          Medico.CRM.NrCRM := '88888888';
          Medico.CRM.UfCRM := tpuf(ufPR);
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2230;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2230.Add do
  begin
    with EvtAfastTemp do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif=ireRetificacao
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := 'A123';

      with infoAfastamento do
      begin
        with iniAfastamento do
        begin
          DtIniAfast := Now;
          codMotAfast := mtvAcidenteDoencaTrabalho;
          infoMesmoMtv := tpNao;
          tpAcidTransito := tpatOutros;
          observacao := 'Campo opcional, salvo quando codMotAfast=21 aí é obrigatória';

          infoAtestado.Clear;

          with infoAtestado.Add do
          begin
            codCID := '1234';
            qtDiasAfast := 13;

            with Emitente do
            begin
              nmEmit := 'João das Neves';
              ideOC := idCRM;
              NrOc := '3690';
              ufOC := ufPR;
            end;
          end;

          // infoCessao opcional usado para afastamento por cessão de funcionário. Ex.: Orgão Público, Sindicatos, etc...
          infoCessao.cnpjCess := '78945612303216';
          infoCessao.infOnus := ocCessionario;

          // infoMandSind opcional para cessão de funcionario para mandato sindical
          infoMandSind.cnpjSind := '12345678901234';
          infoMandSind.infOnusRemun := orEmpregador;
        end;

        // Apenas alteração do MOTIVO de afastamento
        with altAfastamento do
        begin
          dtAltMot := date;
          codMotAnt := '16';
          codMotAfast := '15';
          infoMesmoMtv := tpNao;
          indEfRetroativo := tpSim;
          origAlt := tpOrigemAltAfast(0);
          nrProcJud := '231321321';

          with altEmpr do
          begin
            codCID := '0123';
            qtdDiasAfast := 10;
            nmEmit := 'Nome do emitente na alteração';
            ideOC := tpIdeOC(0);
            NrOc := '12313';
            ufOC := ufSP;
          end;
        end;

        // informações de término do Afastamento
        fimAfastamento.dtTermAfast := Now;
        // fimAfastamento.codMotAfast := '15';
        // fimAfastamento.infoMesmoMtv := tpNao;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2240;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2240.Add do
  begin
    with EvtExpRisco do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        NrRecibo := '654654865656';
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '564545';

      with infoExpRisco do
      begin
        with iniExpRisco do
        begin
          dtCondicao := date;

          InfoAmb.Clear;

          with InfoAmb.Add do
          begin
            codAmb := '654';
            InfoAtiv.dscAtivDes := 'dscAtivDes';

            FatRisco.Clear;

            with FatRisco.Add do
            begin
              codFatRis := '1234567890';
              intConc := 'N/A';
              tecMedicao := 'Técnica de medição';

              with epcEpi do
              begin
                utilizEPC := uEPCUtilizado;
                utilizEPI := uEPIUtilizado;

                epc.Clear;

                with epc.Add do
                begin
                  dscEpc := 'Descrição do EPC 1';
                  eficEpc := tpSim;
                end;

                epi.Clear;

                with epi.Add do
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
          end;
        end;

        // alteração das informações de condições de ambiente de trabalho, opcional
        with altExpRisco do
        begin
          dtCondicao := date;

          with InfoAmb.Add do
          begin
            codAmb := '654';
            InfoAtiv.dscAtivDes := 'dscAtivDes';

            FatRisco.Clear;

            with FatRisco.Add do
            begin
              codFatRis := '1234567890';
              intConc := 'N/A';
              tecMedicao := 'Técnica de medição';

              with epcEpi do
              begin
                utilizEPC := uEPCUtilizado;
                utilizEPI := uEPIUtilizado;

                epc.Clear;

                with epc.Add do
                begin
                  dscEpc := 'Descrição do EPC 2';
                  eficEpc := tpSim;
                end;

                epi.Clear;

                with epi.Add do
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
          end;
        end;

        // fimExpRisco - opcional, informar quando o trabalhador não se sujeitar mais as condições de ambiente informadas anteriormente
        with fimExpRisco do
        begin
          dtFimCondicao := date;

          infoAmb.Clear;

          with InfoAmb.Add do
            codAmb := '897654987';
        end;

        respReg.Clear;

        with respReg.Add do
        begin
          dtIni := Now;
          dtFim := Now;
          NisResp := '12345678901';
          NrOc := '51561561';
          ufOC := ufSP;
        end;

        with respReg.Add do
        begin
          dtIni := Now;
          dtFim := Now;
          NisResp := '12345678901';
          NrOc := '51561561';
          ufOC := ufSP;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2241;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2241.Add do
  begin
    with EvtInsApo do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        NrRecibo := '654654865656';
        TpAmb := taProducaoRestrita;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '564545';

      // InsalPeric - Informações de insalubridade e periculosidade
      with InsalPeric do
      begin
        with iniInsalPeric do
        begin
          DtiniCondicao := date - 60;

          InfoAmb.Clear;

          with InfoAmb.Add do
          begin
            codAmb := '654';

            InfoAtiv.dscAtivDes := 'dscAtivDes';

            FatRisco.Clear;

            with FatRisco.Add do
              codFatRis := '1234567890';
          end;
        end;

        // Opcional - usado para alterações nas condições de trabalho previamente informadas
        // so sera enviado posteriormente quando for alterar um registro
        with altInsalPeric do
        begin
          DtaltCondicao := Date;

          InfoAmb.Clear;

          with InfoAmb.Add do
          begin
            codAmb := '456';
            InfoAtiv.dscAtivDes := 'dscAtivDes';

            FatRisco.Clear;

            with FatRisco.Add do
            begin
              codFatRis := '321';
              intConc := 'N/A';
              tecMedicao := 'Técnica de medição';
            end;
          end;
        end;

        // Opcional - usado quando cessarem as condições de trabalho previamente informadas
        with fimInsalPeric do
        begin
          DtfimCondicao := Date;

          InfoAmb.Clear;
          with InfoAmb.Add do
            codAmb := '123456';
        end;
      end;

      // AposentEsp - Infomações de condições que ensejam aposentadoria especial
      with AposentEsp do
      begin
        with iniAposentEsp do
        begin
          DtiniCondicao := date - 60;

          InfoAmb.Clear;

          with InfoAmb.Add do
          begin
            codAmb := '654';

            InfoAtiv.dscAtivDes := 'dscAtivDes';

            FatRisco.Clear;

            with FatRisco.Add do
              codFatRis := '1234567890';
          end;
        end;

        // Opcional - usado para alterações nas condições de trabalho previamente informadas
        // so sera enviado posteriormente quando for alterar um registro
        with altAposentEsp do
        begin
          DtaltCondicao := Date;

          InfoAmb.Clear;

          with InfoAmb.Add do
          begin
            codAmb := '456';

            InfoAtiv.dscAtivDes := 'dscAtivDes';

            FatRisco.Clear;

            with FatRisco.Add do
            begin
              codFatRis := '321';
              intConc := 'N/A';
              tecMedicao := 'Técnica de medição';
            end;
          end;
        end;

        // Opcional - usado quando cessarem as condições de trabalho previamente informadas
        with fimAposentEsp do
        begin
          DtfimCondicao := Date;

          InfoAmb.Clear;

          with InfoAmb.Add do
            codAmb := '654321';
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2250;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2250.Add do
  begin
    with EvtAvPrevio do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '123456';

      with InfoAvPrevio do
      begin
        // aviso
        if cbAviso.ItemIndex = 0 then
        begin
          DetAvPrevio.dtAvPrv := Now;
          DetAvPrevio.dtPrevDeslig := Now + 30;
          DetAvPrevio.tpAvPrevio := tpTpAvPrevio(0);
          DetAvPrevio.observacao := 'Observacao aviso previo';
        end
        else // cancelamento aviso
        begin
          CancAvPrevio.dtCancAvPrv := Now;
          CancAvPrevio.mtvCancAvPrevio := tpMtvCancAvPrevio(0);
          CancAvPrevio.observacao := 'Observacao cancelamento aviso previo';
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2260;
begin
 with ACBreSocial1.Eventos.NaoPeriodicos.S2260.Add do
  begin
    with EvtConvInterm do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
//        NrRecibo := '65.5454.987798798798';
        TpAmb    := taProducaoRestrita;
        ProcEmi  := TpProcEmi(0);
        VerProc  := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '12345678901234';

      IdeVinculo.CpfTrab   := '04855800392';
      IdeVinculo.NisTrab   := '16179749354';
      IdeVinculo.matricula := '54546';

      with InfoConvInterm do
      begin
        codConv  := '1';
        dtInicio := now;
        dtFim    := now+3;

        jornada.codHorContrat := '1';
        jornada.dscJornada    := 'Descrição da Jornada';

        with localTrab do
        begin
          indLocal := '1';

          with localTrabInterm do
          begin
            TpLograd    := 'Rua';
            DscLograd   := '1o Abril';
            NrLograd    := '10';
            Complemento := 'compl';
            Bairro      := 'Bairro';
            Cep         := '35570000';
            CodMunic    := 3126109;
            UF          := ufMG;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2298;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2298.Add do
  begin
    with EvtReintegr do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '88888888888';
      IdeVinculo.Matricula := '123456';

      with InfoReintegr do
      begin
        tpReint := tpTpReint(0);
        nrProcJud := '999999999';
        dtEfetRetorno := Now + 20;
        dtEfeito := Now;
        indPagtoJuizo := tpSim;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2299;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2299.Add do
  begin
    with EvtDeslig do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      IdeVinculo.CpfTrab := '33333333303';
      IdeVinculo.NisTrab := '11111111111';
      IdeVinculo.Matricula := '123456';

      with InfoDeslig do
      begin
        mtvDeslig := '02';
        DtDeslig := date;
        indPagtoAPI := tpSim;
        dtProjFimAPI := Now;
        pensAlim := paPercentualeValordePensaoAlimenticia;
        percAliment := 25.5;
        vrAlim := 1985.65;

        // Certidão de óbito apenas em caso de morte quando o mtvDeslig for 09 ou 10
        nrCertObito := '0123456789';

        // numero do processo que decidiu o desligamento mtvdeslig = 17
        nrProcTrab := '9632587410';

        indCumprParc := cpaCumprimentoTotal;

        // Obsercação opcional - versão 2.04.01
        observacao := 'Anotações relevantes sobre o desligamento que não tenham campo próprio';

        // Obsercação opcional - versão 2.04.02
        with observacoes.Add do
         observacao := 'Anotações relevantes sobre o desligamento que não tenham campo próprio';

        SucessaoVinc.cnpjEmpregAnt := '12345678912345';

        with VerbasResc do
        begin
          dmDev.Clear;

          with dmDev.Add do
          begin
            ideDmDev := '1234567890';

            with infoPerApur do
            begin
              ideEstabLot.Clear;

              with ideEstabLot.Add do
              begin
                TpInsc := tiCNPJ;
                NrInsc := '12345678901234';
                codLotacao := 'A1234';

                detVerbas.Clear;

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
            end;

            with infoPerAnt do
            begin
              ideADC.Clear;

              with ideADC.Add do
              begin
                dtAcConv := Now;
                tpAcConv := tacAcordoColTrab;
                dtEfAcConv := Now;
                dsc := 'Detalhamento';

                idePeriodo.Clear;

                with IdePeriodo.Add do
                begin
                  perRef := '2017-05';

                  ideEstabLot.Clear;

                  with ideEstabLot.Add do
                  begin
                    TpInsc := tiCNPJ;
                    NrInsc := '12345678901234';
                    codLotacao := 'A1234';

                    detVerbas.Clear;

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
          end;

          procJudTrab.Clear;

          with procJudTrab.Add do
          begin
            tpTrib := tptIRRF;
            nrProcJud := '0123654789';
            codSusp := 1235;
          end;

          with infoMV do
          begin
            indMV := tpIndMV(0);

            remunOutrEmpr.Clear;

            with remunOutrEmpr.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '12345678901234';
              CodCateg := 123;
              vlrRemunOE := 500.84;
            end;
          end;
        end;

        Quarentena.dtFimQuar := Now;

        consigFGTS.Clear;

        with consigFGTS.Add do
        begin
          idConsig := tpSim;
          insConsig := '12345';
          nrContr := '123456';
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2300;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2300.Add do
  begin
    with EvtTSVInicio do
    begin
      Sequencial := 0;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678987654';

      with Trabalhador do
      begin
        CpfTrab := '98765432123';
        NisTrab := '54789632145';
        NmTrab := 'João das Neve';
        Sexo := 'M';
        RacaCor := 1;
        EstCiv := 1;
        GrauInstr := '07';
        nmSoc := 'Nome social';

        with Nascimento do
        begin
          DtNascto := date;
          codMunic := 4153623;
          uf := 'PR';
          PaisNascto := '063';
          PaisNac := '105';
          NmMae := 'Joana das Neve';
          NmPai := 'Jose das Neve';
        end;

        with Documentos do
        begin
          CTPS.NrCtps := '01234567897';
          CTPS.SerieCtps := '00001';
          CTPS.UfCtps := 'PR';

          RIC.NrRic := '10203040506070';
          RIC.OrgaoEmissor := 'Orgão Emissor';
          RIC.DtExped := date;

          RG.NrRg := '73062584';
          RG.OrgaoEmissor := 'SSP';
          RG.DtExped := date;

          RNE.NrRne := '01020304050607';
          RNE.OrgaoEmissor := 'Orgao Emissor';
          RNE.DtExped := date;

          OC.NrOc := '74108520963012';
          OC.OrgaoEmissor := 'CRProfissao';
          OC.DtExped := date;
          OC.DtValid := date;

          CNH.nrRegCnh := '123654789632';
          CNH.DtExped := date;
          CNH.ufCnh := ufPR;
          CNH.DtValid := date;
          CNH.dtPriHab := date;
          CNH.categoriaCnh := cnAB;
        end;

        with Endereco do
        begin
          Brasil.TpLograd := 'R';
          Brasil.DscLograd := 'Rua Parmenides';
          Brasil.NrLograd := '123456';
          Brasil.Complemento := 'fundos';
          Brasil.Bairro := 'Jd Filosofia';
          Brasil.Cep := '88888888';
          Brasil.codMunic := 4141414;
          Brasil.uf := ufPR;

          // Dados de trabalhador estrangeiro
          Exterior.PaisResid := '063';
          Exterior.DscLograd := 'St. Abbey Road';
          Exterior.NrLograd := '123456';
          Exterior.Complemento := 'apto 010';
          Exterior.Bairro := 'RubberSoul';
          Exterior.NmCid := 'Buenos Aires';
          Exterior.CodPostal := '987654';
        end;

        with TrabEstrangeiro do
        begin
          DtChegada := date;
          ClassTrabEstrang := ctVistoPermanente;
          CasadoBr := 'S';
          FilhosBr := 'N';
        end;

        // Dados de trabalhador com deficiencia
        with InfoDeficiencia do
        begin
          DefFisica := tpNao;
          DefVisual := tpNao;
          DefAuditiva := tpNao;
          DefMental := tpNao;
          DefIntelectual := tpNao;
          ReabReadap := tpSim;
          observacao := 'sem deficiencia';
        end;

        Dependente.Clear;

        with Dependente.Add do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          DtNascto := date;
          cpfDep := '99999999909';
          depIRRF := tpSim;
          depSF := tpNao;
        end;

        with Dependente.Add do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '99999999909';
          depIRRF := tpSim;
          depSF := tpNao;
        end;

        with Contato do
        begin
          FonePrinc := '91067240';
          FoneAlternat := '91067240';
          EmailPrinc := 'TESTE@email.com.br';
          EmailAlternat := 'teste@teste.com';
        end;
      end;

      with infoTSVInicio do
      begin
        CodCateg := 101;
        dtInicio := date;
        natAtividade := navUrbano;

        with infoComplementares do
        begin
          cargoFuncao.CodCargo := '001';
          cargoFuncao.CodFuncao := '001';

          Remuneracao.VrSalFx := 1200.00;
          Remuneracao.UndSalFixo := sfPorMes;
          Remuneracao.DscSalVar := 'Comissão de 1,2% sobre a venda do mês';

          FGTS.OpcFGTS := ofOptante;
          FGTS.DtOpcFGTS := date;

          // dados da empresa de origem do dirigente sindical
          with infoDirSind do
          begin
            categOrig := 111;
            cnpjOrigem := '12345678901234';
            dtAdmOrig := date;
            matricOrig := 'A1234';
          end;

          // Informações de trabalhador cedido, devem ser preenchidas exclusivamente pelo cessionário
          with infoTrabCedido do
          begin
            categOrig := 111;
            cnpjCednt := '12345678901234';
            matricCed := 'B4321';
            dtAdmCed := date;
            TpRegTrab := trCLT;
            TpRegPrev := rpRGPS;
            infOnus := ocCedente;
          end;

          with infoEstagiario do
          begin
            natEstagio := neObrigatiorio;
            nivEstagio := nvSuperior;
            areaAtuacao := 'Direito';
            nrApol := '123456';
            vlrBolsa := 600.00;
            dtPrevTerm := IncMonth(date, 12);

            with instEnsino do
            begin
              cnpjInstEnsino := '12345678901234';
              NmRazao := 'Nome da Instituição de Ensino';
              DscLograd := 'R Pitagoras';
              NrLograd := '1618';
              Bairro := 'Bairro Educacional';
              Cep := '86086086';
              codMunic := 4141414;
              uf := ufPR;
            end;

            with ageIntegracao do
            begin
              cnpjAgntInteg := '12345678901234';
              NmRazao := 'Nome da empresa';
              DscLograd := 'R Adam Smith';
              NrLograd := '9999';
              Bairro := 'Bairro Empresarial';
              Cep := '86086086';
              codMunic := 4141414;
              uf := ufPR;
            end;

            supervisorEstagio.cpfSupervisor := '88888888801';
            supervisorEstagio.nmSuperv := 'Pedro das Pedras';
          end;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2306;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2306.Add do
  begin
    with EvtTSVAltContr do
    begin
      Sequencial := 0;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := '12345678987654';

      IdeTrabSemVinc.CpfTrab := '12345678901';
      IdeTrabSemVinc.NisTrab := '00000000000';
      IdeTrabSemVinc.CodCateg := 555;

      with infoTSVAlteracao do
      begin
        dtAlteracao := date;
        natAtividade := navUrbano;

        with infoComplementares do
        begin
          cargoFuncao.CodCargo := '001';
          cargoFuncao.CodFuncao := '001';

          Remuneracao.VrSalFx := 1200.00;
          Remuneracao.UndSalFixo := sfPorMes;
          Remuneracao.DscSalVar := 'Comissão de 1,2% sobre a venda do mês';

          with infoEstagiario do
          begin
            natEstagio:= neObrigatiorio;
            nivEstagio := nvSuperior;
            areaAtuacao := 'Direito';
            nrApol := '123456';
            vlrBolsa := 600.00;
            dtPrevTerm := IncMonth(date, 12);

            with instEnsino do
            begin
              cnpjInstEnsino := '12345678998765';
              NmRazao := 'Nome da Instituição de Ensino';
              DscLograd := 'R Pitagoras';
              NrLograd := '1618';
              Bairro := 'Bairro Educacional';
              Cep := '86086086';
              codMunic := 4141414;
              uf := ufPR;
            end;

            with ageIntegracao do
            begin
              cnpjAgntInteg := '98765432145678';
              NmRazao := 'Nome da empresa';
              DscLograd := 'R Adam Smith';
              NrLograd := '9999';
              Bairro := 'Bairro Empresarial';
              Cep := '86086086';
              codMunic := 4141414;
              uf := ufPR;
            end;

            supervisorEstagio.cpfSupervisor := '12345678901';
            supervisorEstagio.nmSuperv := 'Pedro das Pedras';
          end;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2399;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2399.Add do
  begin
    with EvtTSVTermino do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      IdeTrabSemVinc.CpfTrab := '12345678987';
      IdeTrabSemVinc.NisTrab := '98765432123';
      IdeTrabSemVinc.CodCateg := 111;

      with InfoTSVTermino do
      begin
        dtTerm := date;
        mtvDesligTSV := '02';

        with verbasResc do
        begin
          dmDev.Clear;

          with dmDev.Add do
          begin
            ideDmDev := '012345';

            ideEstabLot.Clear;

            with ideEstabLot.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '12345678987654';
              codLotacao := 'A1234';

              detVerbas.Clear;

              with detVerbas.Add do
              begin
                CodRubr := 'Pg123';
                ideTabRubr := 'A01';
                qtdRubr := 2;
                fatorRubr := 50.25;
                vrUnit := 152.35;
                vrRubr := 304.70;
              end;

              with infoSaudeColet do
              begin
                detOper.Clear;

                with detOper.Add do
                begin
                  cnpjOper := '89652048000195';
                  regANS := '123456';
                  vrPgTit := 1500.65;

                  detPlano.Clear;

                  with detPlano.Add do
                  begin
                    cpfDep := '55555555555';
                    nmDep := 'Nome do Dependente';
                    DtNascto := Now;
                    vlrPgDep := 125.36;
                  end;
                end;
              end;

              infoAgNocivo.grauExp := ge1;
              infoSimples.indSimples := idsIntegralmente;
            end;
          end;

          procJudTrab.Clear;

          with procJudTrab.Add do
          begin
            tpTrib := tpTpTributo(0);
            nrProcJud := '123456789';
            codSusp := 123456;
          end;

          with infoMV do
          begin
            indMV := tpIndMV(0);

            with remunOutrEmpr.Add do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '14236547000195';
              CodCateg := 111;
              vlrRemunOE := 2500.12;
            end;
          end;
        end;

        Quarentena.dtFimQuar := date;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial2400;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2400.Add do
  begin
    with evtCdBenPrRP do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        TpAmb := taProducaoRestrita;
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      with ideBenef do
      begin
        cpfBenef := '12345678910';
        nmBenefic := 'Nome do beneficiario';

        with dadosBenef do
        begin
          with dadosNasc do
          begin
            DtNascto := date;;
            codMunic := 4153623;
            uf := 'PR';
            PaisNascto := '063';
            PaisNac := '105';
            NmMae := 'Joana das Neve';
            NmPai := 'Jose das Neve';
          end;

          with endereco do
          begin
            with Brasil do
            begin
              TpLograd := 'R';
              DscLograd := 'Rua Parmenides';
              NrLograd := '123456';
              Complemento := 'fundos';
              Bairro := 'Jd Filosofia';
              Cep := '88888888';
              codMunic := 4141414;
              uf := ufPR;
            end;

            // Dados de trabalhador estrangeiro
            with Exterior do
            begin
              PaisResid := '063';
              DscLograd := 'St. Abbey Road';
              NrLograd := '123456';
              Complemento := 'apto 010';
              Bairro := 'RubberSoul';
              NmCid := 'Buenos Aires';
              CodPostal := '987654';
            end;
          end;
        end;
      end;

      with infoBeneficio do
      begin
        tpPlanRP := prpPlanoPrevidenciarioOuUnico;

        with iniBeneficio do
        begin
          tpBenef := 1;
          nrBenefic := '3156189132131';
          dtIniBenef := Now;
          vrBenef := 1500.32;
          infoPenMorte.idQuota := '1521651651';
          infoPenMorte.cpfInst := '12345678910';
        end;

        with altBeneficio do
        begin
          tpBenef := 1;
          nrBenefic := '3156189132131';
          dtIniBenef := Now;
          vrBenef := 1500.32;
          infoPenMorte.idQuota := '1521651651';
          infoPenMorte.cpfInst := '12345678910';
        end;

        with fimBeneficio do
        begin
          tpBenef := 1;
          nrBenefic := '3156189132131';
          dtFimBenef := Now;
          mtvFim := 1;
        end;
      end;
    end;
  end;
end;

procedure TFExemploEsocial.GerareSocial3000;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S3000.Add do
  begin
    with EvtExclusao do
    begin
      Sequencial := 0;

      IdeEvento.TpAmb := taProducaoRestrita;
      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tpTpInsc(1);
      IdeEmpregador.NrInsc := '0123456789';

      with InfoExclusao do
      begin
        tpEvento := TTipoEvento(teS2100);
        nrRecEvt := '12345789987654321';

        ideTrabalhador.CpfTrab := '12345678950';
        ideTrabalhador.NisTrab := '12345678901';

        IdeFolhaPagto.IndApuracao := tpIndApuracao(0);
        IdeFolhaPagto.perApur := '2015-05';
      end;
    end;
  end;
end;

function TFExemploEsocial.GetTipoOperacao: TModoLancamento;
begin
  case rdgOperacao.ItemIndex of
    1: Result := mlAlteracao;
    2: Result := mlExclusao;
  else
    Result := mlInclusao;
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

procedure TFExemploEsocial.btnGerarEnviarClick(Sender: TObject);
var
  i: Integer;
begin
  if chkClear.Checked then
    LimparDocsPasta;

  try
    SelecionaEventos;
    ACBreSocial1.AssinarEventos;

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
          Add(' - dhRecepcao..............: ' +
            DateTimeToStr(dadosRecLote.dhRecepcao));
          Add(' - versaoAplicativoRecepcao: ' +
            dadosRecLote.versaoAplicRecepcao);
          Add(' - protocoloEnvio..........: ' + dadosRecLote.Protocolo);
        end
        else
        begin
          for i := 0 to Status.Ocorrencias.Count - 1 do
          begin
            with Status.Ocorrencias.Items[i] do
            begin
              Add(' Ocorrencia ' + IntToStr(i));
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

procedure TFExemploEsocial.btnEnviarClick(Sender: TObject);
var
  i: Integer;
begin
  if chkClear.Checked then
    LimparDocsPasta;

  try
    ACBreSocial1.Enviar(TESocialGrupo(rdgGrupo.ItemIndex + 1));

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
          Add(' - dhRecepcao..............: ' +
            DateTimeToStr(dadosRecLote.dhRecepcao));
          Add(' - versaoAplicativoRecepcao: ' +
            dadosRecLote.versaoAplicRecepcao);
          Add(' - protocoloEnvio..........: ' + dadosRecLote.Protocolo);
        end
        else
        begin
          for i := 0 to Status.Ocorrencias.Count - 1 do
          begin
            with Status.Ocorrencias.Items[i] do
            begin
              Add(' Ocorrencia ' + IntToStr(i));
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
  i, J: Integer;
  evtS5001: TS5001;
  evtS5002: TS5002;
  evtS5011: TS5011;
  evtS5012: TS5012;
begin
  Protocolo := '';
  if not(InputQuery('WebServices: Consulta Protocolo', 'Protocolo', Protocolo))
  then
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
          Add(' - dhRecepcao..............: ' +
            DateTimeToStr(dadosRecLote.dhRecepcao));
          Add(' - versaoAplicativoRecepcao: ' +
            dadosRecLote.versaoAplicRecepcao);
          Add(' - protocoloEnvio..........: ' + dadosRecLote.Protocolo);

          for i := 0 to retEventos.Count - 1 do
          begin
            Add('Processamento');
            Add(' - cdResposta.........: ' +
              IntToStr(retEventos.Items[i].Processamento.cdResposta));
            Add(' - descResposta.......: ' + retEventos.Items[i]
              .Processamento.descResposta);
            Add(' - versaoAplicProcLote: ' + retEventos.Items[i]
              .Processamento.versaoAplicProcLote);
            Add(' - dhProcessamento....: ' + DateTimeToStr(retEventos.Items[i]
              .Processamento.dhProcessamento));

            if retEventos.Items[i].Processamento.Ocorrencias.Count > 0 then
            begin
              Add('Ocorrencias do Processamento');
              for J := 0 to retEventos.Items[i].Processamento.Ocorrencias.
                Count - 1 do
              begin
                Add(' Ocorrencia ' + IntToStr(J));
                Add('   Código.....: ' +
                  IntToStr(retEventos.Items[i].Processamento.Ocorrencias.Items
                  [J].Codigo));
                Add('   Descrição..: ' + retEventos.Items[i]
                  .Processamento.Ocorrencias.Items[J].Descricao);
                Add('   Tipo.......: ' +
                  IntToStr(retEventos.Items[i].Processamento.Ocorrencias.Items
                  [J].Tipo));
                Add('   Localização: ' + retEventos.Items[i]
                  .Processamento.Ocorrencias.Items[J].Localizacao);
              end;
            end;

            for J := 0 to retEventos.Items[i].tot.Count - 1 do
            begin
              Add(' Tot ' + IntToStr(J));
              Add('   Tipo.........: ' + retEventos.Items[i].tot[J].Tipo);
              case retEventos.Items[i].tot[J].Evento.TipoEvento of
                teS5001:
                  begin
                    evtS5001 := TS5001(retEventos.Items[i].tot[J].Evento.GetEvento);
                    Add('   Id...........: ' + evtS5001.EvtBasesTrab.Id);
                    Add('   nrRecArqBase.: ' +
                      evtS5001.EvtBasesTrab.IdeEvento.nrRecArqBase);
                  end;
                teS5002:
                  begin
                    evtS5002 := TS5002(retEventos.Items[i].tot[J].Evento.GetEvento);
                    Add('   Id...........: ' + evtS5002.EvtirrfBenef.Id);
                    Add('   nrRecArqBase.: ' +
                      evtS5002.EvtirrfBenef.IdeEvento.nrRecArqBase);
                  end;
                teS5011:
                  begin
                    evtS5011 := TS5011(retEventos.Items[i].tot[J].Evento.GetEvento);
                    Add('   Id...........: ' + evtS5011.EvtCS.Id);
                    Add('   nrRecArqBase.: ' +
                      evtS5011.EvtCS.IdeEvento.nrRecArqBase);
                  end;
                teS5012:
                  begin
                    evtS5012 := TS5012(retEventos.Items[i].tot[J].Evento.GetEvento);
                    Add('   Id...........: ' + evtS5012.EvtIrrf.Id);
                    Add('   nrRecArqBase.: ' +
                      evtS5012.EvtIrrf.IdeEvento.nrRecArqBase);
                  end;
              end;
            end;

            Add('Recibo');
            Add(' - nrRecibo: ' + retEventos.Items[i].Recibo.NrRecibo);
            Add(' - hash....: ' + retEventos.Items[i].Recibo.hash);
          end;

        end
        else
        begin
          for i := 0 to Status.Ocorrencias.Count - 1 do
          begin
            with Status.Ocorrencias.Items[i] do
            begin
              Add(' Ocorrencia ' + IntToStr(i));
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

procedure TFExemploEsocial.ACBreSocial1TransmissaoEventos
  (const AXML: AnsiString; ATipo: TeSocialEventos);
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
  if (cbS2260.Checked) then
    GerareSocial2260;

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
  OpenDialog1.Filter :=
    'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBreSocial1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
    ACBreSocial1.Eventos.LoadFromFile(OpenDialog1.FileName);

  MemoResp.Lines.Add('XML de Eventos Carregado com Sucesso!');
  pgWebservice.ActivePageIndex := 3;
end;

procedure TFExemploEsocial.btnCarregarINIClick(Sender: TObject);
var
  i: Integer;
begin
  OpenDialog1.Title := 'Selecione o Evento (Arquivo INI)';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBreSocial1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
    ACBreSocial1.Eventos.LoadFromINI(OpenDialog1.FileName);

  MemoResp.Clear;
  MemoResp.Lines.Clear;
  MemoResp.Lines.Add('INI de Eventos Carregado com Sucesso!');
  MemoResp.Lines.Add(' ');

  for I := 0 to ACBreSocial1.Eventos.Gerados.Count -1 do
  begin
    MemoResp.Lines.Add('Tipo Evento.: ' + TipoEventoToStr(ACBreSocial1.Eventos.Gerados.Items[i].TipoEvento));
    MemoResp.Lines.Add('Evento Salvo: ' + ACBreSocial1.Eventos.Gerados.Items[i].PathNome);
  end;

  pgWebservice.ActivePageIndex := 2;
end;

end.
