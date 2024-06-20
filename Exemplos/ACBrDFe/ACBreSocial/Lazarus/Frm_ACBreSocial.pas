{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit Frm_ACBreSocial;

{$MODE Delphi}

interface

uses
  IniFiles, LCLIntf, LCLType, SysUtils, Variants, Classes,
  {$IFDEF WINDOWS}ShellApi,{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, Spin, Buttons, ExtCtrls,
  SynEdit, SynHighlighterXML,
  ACBrUtil, ACBrBase, ACBrDFe,
  pcnConversao, pcesConversaoeSocial,
  ACBreSocial;

type

  { TfrmACBreSocial }

  TfrmACBreSocial = class(TForm)
    pnlMenus: TPanel;
    pnlCentral: TPanel;
    PageControl1: TPageControl;
    SynXMLSyn1: TSynXMLSyn;
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
    TabSheet13: TTabSheet;
    sbPatheSocial: TSpeedButton;
    Label35: TLabel;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPatheSocial: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPatheSocial: TEdit;
    cbxSepararPorModelo: TCheckBox;
    btnSalvarConfig: TBitBtn;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    pgcBotoes: TPageControl;
    pgRespostas: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TSynEdit;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwDocumento: TTreeView;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    OpenDialog1: TOpenDialog;
    ACBreSocial1: TACBreSocial;
    gbDadosEmpresa: TGroupBox;
    Label7: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    edtIdEmpregador: TEdit;
    edtIdTransmissor: TEdit;
    cbTEmpregador: TComboBox;
    tbsEventosTabela: TTabSheet;
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
    tbsEventosPeriodicos: TTabSheet;
    cbS1200: TCheckBox;
    cbS1202: TCheckBox;
    cbS1207: TCheckBox;
    cbS1210: TCheckBox;
    cbS1250: TCheckBox;
    cbS1260: TCheckBox;
    cbS1270: TCheckBox;
    cbS1280: TCheckBox;
    cbS1298: TCheckBox;
    cbS1299: TCheckBox;
    cbS1300: TCheckBox;
    tbsEventosNaoPeriodicos: TTabSheet;
    cbS2190: TCheckBox;
    cbS2200: TCheckBox;
    cbS2205: TCheckBox;
    cbS2206: TCheckBox;
    cbS2210: TCheckBox;
    cbS2220: TCheckBox;
    cbS2221: TCheckBox;
    cbS2230: TCheckBox;
    cbS2231: TCheckBox;
    cbS2240: TCheckBox;
    cbS2245: TCheckBox;
    cbS2250: TCheckBox;
    cbAviso: TComboBox;
    cbs2260: TCheckBox;
    cbS2298: TCheckBox;
    cbS2299: TCheckBox;
    cbS2300: TCheckBox;
    cbS2306: TCheckBox;
    cbS2399: TCheckBox;
    cbS2400: TCheckBox;
    cbS2405: TCheckBox;
    cbS2410: TCheckBox;
    cbS2416: TCheckBox;
    cbS2418: TCheckBox;
    cbS2420: TCheckBox;
    cbS2500: TCheckBox;
    cbS2501: TCheckBox;
    cbS3000: TCheckBox;
    tsComandos: TTabSheet;
    btnGerar: TButton;
    btnCarregarXML: TButton;
    btnCarregarINI: TButton;
    btnEnviar: TButton;
    btnGerarEnviar: TButton;
    btnConsultar: TButton;
    btnConsIdeEveEmp: TButton;
    btnDownloadEventos: TButton;
    btnConsIdeEveTab: TButton;
    btnConsIdeEveTrab: TButton;
    tsFormaEnvio: TTabSheet;
    rdgGrupo: TRadioGroup;
    rdgOperacao: TRadioGroup;
    chkClear: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPatheSocialClick(Sender: TObject);
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
    procedure btnGerarClick(Sender: TObject);
    procedure btnCarregarXMLClick(Sender: TObject);
    procedure btnCarregarINIClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnGerarEnviarClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsIdeEveEmpClick(Sender: TObject);
    procedure btnConsIdeEveTabClick(Sender: TObject);
    procedure btnConsIdeEveTrabClick(Sender: TObject);
    procedure btnDownloadEventosClick(Sender: TObject);
    procedure ACBreSocial1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ACBreSocial1StatusChange(Sender: TObject);
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
    procedure GerareSocial2221;
    procedure GerareSocial2230;
    procedure GerareSocial2231;
    procedure GerareSocial2240;
    procedure GerareSocial2245;
    procedure GerareSocial2250;
    procedure GerareSocial2260;
    procedure GerareSocial2298;
    procedure GerareSocial2299;
    procedure GerareSocial2300;
    procedure GerareSocial2306;
    procedure GerareSocial2399;
    procedure GerareSocial2400;
    procedure GerareSocial2405;
    procedure GerareSocial2410;
    procedure GerareSocial2416;
    procedure GerareSocial2418;
    procedure GerareSocial2420;
    procedure GerareSocial2500;
    procedure GerareSocial2501;
    procedure GerareSocial3000;

    procedure LimparDocsPasta;
    procedure SelecionaEventos;

    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure AtualizarSSLLibsCombo;

    function VersaoDFx: TVersaoeSocial;
  public
    { Public declarations }
  end;

var
  frmACBreSocial: TfrmACBreSocial;

implementation

uses
  math, TypInfo, DateUtils, blcksock, Grids,
  Printers, pcnAuxiliar, pcesS5001, pcesS5002, pcesS5011, pcesS5012,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBreSocialEventos, ACBreSocialConfiguracoes, Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.lfm}

{ TfrmACBreSocial }

function TfrmACBreSocial.VersaoDFx: TVersaoeSocial;
begin
  Result := ACBreSocial1.Configuracoes.Geral.VersaoDF;
end;

procedure TfrmACBreSocial.GerareSocial1000;
begin
  with ACBreSocial1.Eventos.Iniciais.S1000.New do
  begin
    with evtInfoEmpregador do
    begin
      sequencial := 0;
      modoLancamento := GetTipoOperacao;

      with ideEvento do
      begin
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with infoEmpregador do
      begin
        with idePeriodo do
        begin
          iniValid := '2015-05';
          fimValid := '2099-12';
        end;

        with infoCadastro do
        begin
          if Checb_ZeraBase.Checked then
          begin
            if VersaoDFx <= ve02_05_00 then
              nmRazao := 'RemoverEmpregadorDaBaseDeDadosDaProducaoRestrita';
            classTrib := ct00;
          end
          else
          begin
            if VersaoDFx <= ve02_05_00 then
              nmRazao := 'Empresa Teste';
            classTrib := ct01;
          end;

          if VersaoDFx <= ve02_05_00 then
            natJurid := '0001';
          indCoop := tpIndCoop(1);
          indConstr := tpIndConstr(1);
          indDesFolha := tpIndDesFolha(2);
          indPorte := tpNao;
          indOptRegEletron := tpIndOptRegEletron(1);

          if VersaoDFx <= ve02_05_00 then
          begin
            indEtt := snfSim;
            nrRegEtt := '';
          end;

          if VersaoDFx <= ve02_05_00 then
          begin
            with infoOp do
            begin
              nrSiafi := '12345';

              with infoEnte do
              begin
                nmEnte := 'Ente federativo teste';
                uf := 'SP';
                vrSubteto := 100.00;
              end;
            end;
          end;

          with dadosIsencao do
          begin
            ideMinLei := 'Sigla Min';
            nrCertif := '1111';
            dtEmisCertif := date;
            dtVencCertif := date;
            nrProtRenov := '10';
            dtProtRenov := date;
            dtDou := date;
            pagDou := '111';
          end;

          if VersaoDFx <= ve02_05_00 then
            with Contato do
            begin
              nmCtt := 'Contato 1';
              cpfCtt := '00000222220';
              foneFixo := '34335856';
              foneCel := '991524587';
              email := 'testecontato@testecontato.com';
            end;

          with infoOrgInternacional do
          begin
            indAcordoIsenMulta := tpIndAcordoIsencaoMulta(1);
          end;

          if VersaoDFx <= ve02_05_00 then
          begin
            softwareHouse.Clear;

            with softwareHouse.New do
            begin
              cnpjSoftHouse := '00000000000000';
              nmRazao := 'SoftwareHouse Teste';
              nmCont := 'Soft Contato';
              telefone := '34335856';
              email := 'teste@teste.com';
            end;


          end;
        end;

        with novaValidade do
        begin
          iniValid := '2014-05';
          fimValid := '2099-12';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1005;
begin
  with ACBreSocial1.Eventos.Iniciais.S1005.New do
  begin
    with evtTabEstab do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      with ideEvento do
      begin
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with infoEstab do
      begin
        with ideEstab do
        begin
          tpInsc := tiCNPJ;
          nrInsc := '00000000000000';
          iniValid := '2015-05';
          fimValid := '2099-12';
        end;

        with DadosEstab do
        begin
          cnaePrep := '0000000';

          with aliqGilrat do
          begin
            aliqRat := arat1;
            fap := 1.5;

            if VersaoDFx <= ve02_05_00 then
              aliqRatAjust := 2.5;
 {
            with ProcAdmJudRat do
            begin
              tpProc := tpTpProc(1);
              nrProc := '20150512';
              codSusp := '1';
            end;

            with procAdmJudFap do
            begin
              tpProc := tpTpProc(1);
              nrProc := '20150512';
              codSusp := '2';
            end;
}
          end;

          with infoCaepf do
          begin
            tpCaepf := tcContrIndividual;
          end;

          with infoObra do
          begin
            indSubstPatrObra := tpIndSubstPatronalObra(1);
          end;

          with infoTrab do
          begin


            with infoApr do
            begin

{
              nrProcJud := '20150612';
}
              if VersaoDFx <= ve02_05_00 then
                contEntEd := snfSim;

              infoEntEduc.Clear;

              with infoEntEduc.New do
                nrInsc := '0123456789';
            end;
{
            with infoPCD do
            begin
              if VersaoDFx <= ve02_05_00 then
                contPCD := tpContPCD(9);

              nrProcJud := '20160131';
            end;
}
          end;

          with novaValidade do
          begin
            iniValid := '2014-05';
            fimValid := '2099-12';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1010;
begin
  with ACBreSocial1.Eventos.Tabelas.S1010.New do
  begin
    with evtTabRubrica do
    begin
      sequencial := 0;
      modoLancamento := GetTipoOperacao;

      with ideEvento do
      begin
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with infoRubrica do
      begin
        with ideRubrica do
        begin
          codRubr := '5445';
          ideTabRubr := '100000';
          iniValid := '2015-05';
          fimValid := '2015-06';
        end;

        with dadosRubrica do
        begin
          dscRubr := 'Teste de S-1010';
          natRubr := 1022;
          tpRubr := tpTpRubr(1);
          codIncCP := tpCodIncCP(1);
          codIncIRRF := tpCodIncIRRF(1);
          codIncFGTS := tpCodIncFGTS(1);


          codIncCPRP := cicpNaoeBasedeCalculodeContribuicoesDevidasaoRPPSRegimeMilitar;

          observacao := 'Rubrica Teste';
 {
          ideProcessoCP.Clear;

          with ideProcessoCP.New do
          begin
            nrProc := '1020';
            extDecisao := tpExtDecisao(1);
            codSusp := '1';
          end;

          ideProcessoIRRF.Clear;

          with ideProcessoIRRF.New do
          begin
            nrProc := '1020';
            codSusp := '2';
          end;

          ideProcessoFGTS.Clear;

          with ideProcessoFGTS.New do
          begin
            nrProc := '50740';
          end;

          if VersaoDFx <= ve02_05_00 then
          begin
            ideProcessoSIND.Clear;

            with ideProcessoSIND.New do
            begin
              nrProc := '123456';
            end;
          end;
}
        end;

        if (ModoLancamento = mlAlteracao) then
        begin
          with novaValidade do
          begin
            iniValid := '2015-05';
            fimValid := '2099-12';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1020;
begin
  with ACBreSocial1.Eventos.Tabelas.S1020.New do
  begin
    with evtTabLotacao do
    begin
      sequencial := 0;
      modoLancamento := GetTipoOperacao;

      with ideEvento do
      begin
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with infoLotacao do
      begin
        with ideLotacao do
        begin
          codLotacao := '300000';
          iniValid := '2015-06';
          fimValid := '2099-12';
        end;

        with dadosLotacao do
        begin
          tpLotacao := '01';
          tpInsc := tiCNPJ;
          nrInsc := '00000000000000';

          with fPasLotacao do
          begin
            fpas := '515';
            codTercs := '0015';
            codTercsSusp := '0506';
{
            with infoProcJudTerceiros do
            begin
              procJudTerceiro.Clear;

              with procJudTerceiro.New do
              begin
                codTerc := '1111';
                nrProcJud := '1234567891239-1345';
                codSusp := '1';
              end;
            end;
}
          end;

          with infoEmprParcial do
          begin
            tpInscContrat := tpTpInscContratante(0);
            nrInscContrat := '00000000000000';
            tpInscProp := TpTpInscProp(0);
            nrInscProp := '654234523416';
          end;

          if VersaoDFx > ve02_05_00 then
            with dadosOpPort do
            begin
              aliqRat := arat3;
              fap := 1.0;
            end;
        end;

        with novaValidade do
        begin
          iniValid := '2015-06';
          fimValid := '2099-12';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1030;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.Tabelas.S1030.New do
  begin
    with evtTabCargo do
    begin
      sequencial := 0;
      modoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

          end;
        end;

        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1035;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.Tabelas.S1035.New do
  begin
    with evtTabCarreira do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with InfoCarreira do
      begin
        ideCarreira.codCarreira := '1';
        ideCarreira.IniValid := '2015-05';
        ideCarreira.IniValid := '2099-12';

        dadosCarreira.dscCarreira := 'Juiz';
        dadosCarreira.leiCarr := 'lei89489/77';
        dadosCarreira.dtLeiCarr := Now;


        NovaValidade.IniValid := '2015-05';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1040;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.Tabelas.S1040.New do
  begin
    with EvtTabFuncao do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

procedure TfrmACBreSocial.GerareSocial1050;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.Tabelas.S1050.New do
  begin
    with EvtTabHorContratual do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

          with horarioIntervalo.New do
          begin
            durInterv := 90;
            iniInterv := '1200';
            termInterv := '1330';
          end;

          with horarioIntervalo.New do
          begin
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

procedure TfrmACBreSocial.GerareSocial1060;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.Tabelas.S1060.New do
  begin
    with EvtTabAmbiente do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with infoAmbiente do
      begin
        ideAmbiente.codAmb := '123456';
        ideAmbiente.IniValid := '2015-05';
        ideAmbiente.FimValid := '2099-12';

        with dadosAmbiente do
        begin
          nmAmb      := 'AMBIENTE 01';
          dscAmb     := 'DESCRICAO DO AMBIENTE';
          localAmb   := laEstabProprioEmpregador;
          TpInsc     := tiCNPJ;
          NrInsc     := '12345678000112';
          CodLotacao := '';
        end;

        NovaValidade.IniValid := '2015-06';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1070;
begin
  with ACBreSocial1.Eventos.Tabelas.S1070.New do
  begin
    with EvtTabProcesso do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

          with infoSusp.New do
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

procedure TfrmACBreSocial.GerareSocial1080;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.Tabelas.S1080.New do
  begin
    with EvtTabOperPortuario do
    begin
      Sequencial := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

procedure TfrmACBreSocial.GerareSocial1200;
begin
  with ACBreSocial1.Eventos.Periodicos.S1200.New do
  begin
    with evtRemun do
    begin
      Sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo  := '4564654'; Numero do recibo que será retificado.
        IndApuracao := iapuMensal;
        perApur := '2015-05';
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

          with remunOutrEmpr.New do
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
            tpInscAnt := tiCNPJ;
            cnpjEmpregAnt := '12345678901234';
            MatricAnt := '123';
            DtAdm := Now;
            observacao := 'obs sucessao vinc';
          end;
        end;

        // os dados abaixo só devem ser informados em caso do processo existir e houver decisão que incida sobre as
        // contribuições
        procJudTrab.Clear;

        with procJudTrab.New do
        begin
          tpTrib := tptPrevidenciaria;
          nrProcJud := '95135703320156150258';
          codSusp := '123456789';
        end;
      end;

      dmDev.Clear;

      with dmDev.New do
      begin
        ideDmDev := '1';
        CodCateg := 111;

        if VersaoDFx > veS01_00_00 then
        begin
          indRRA:= snfSim;
          with infoRRA do
          begin
            tpProcRRA := tppAdministrativo;
            nrProcRRA := '123';
            descRRA := 'Rendimentos Receb Acum.';
            qtdMesesRRA := 5;
            despProcJud.vlrDespCustas:= 0;
            despProcJud.vlrDespAdvogados := 0;
          end;
        end;

        with infoPerApur.ideEstabLot.New do
        begin
          TpInsc := tiCNPJ;
          NrInsc := '01234567891234';
          codLotacao := 'SACI54321';
          qtdDiasAv := 22;

          remunPerAnt.Clear;

          with remunPerApur.New do
          begin
            Matricula := 'A1234';
            indSimples := idsIntegralmente;

            itensRemun.Clear;

            with itensRemun.New do
            begin
              CodRubr := '987654';
              ideTabRubr := 'E380';
              qtdRubr := 100;
              fatorRubr := 50;
              vrUnit := 3296.35;
              vrRubr := 3330.30;
            end;

            infoSaudeColet.detOper.Clear;

            with infoSaudeColet.detOper.New do
            begin
              cnpjOper := '01234567891234';
              regANS := 'A1B2C3';
              vrPgTit := 1.50;

              detPlano.Clear;

              with detPlano.New do
              begin
                tpDep := tdConjuge;
                cpfDep := '01234567891';
                nmDep := 'José das Areias';
                DtNascto := date;
                vlrPgDep := 0.75;
              end;
            end;

            infoAgNocivo.grauExp := ge1;
          end;
        end;

        infoPerAnt.ideADC.Clear;

        with infoPerAnt.ideADC.New do
        begin
          dtAcConv := Now;
          tpAcConv := tacLegislacaoFederalEstadualMunicipalDistrital;
          dtEfAcConv := Now;
          compAcConv := '2017-01';
          dsc := 'Dissídio';

          idePeriodo.Clear;

          with IdePeriodo.New do
          begin
            perRef := '2015-04';

            ideEstabLot.Clear;

            with ideEstabLot.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '01234567891234';
              codLotacao := 'TESTE123';

              remunPerAnt.Clear;

              with remunPerAnt.New do
              begin
                Matricula := 'A1234';
                indSimples := idsIntegralmente;

                itensRemun.Clear;

                with itensRemun.New do
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

        with infoTrabInterm.New do
          codConv := '123456';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1202;
begin
  with ACBreSocial1.Eventos.Periodicos.S1202.New do
  begin
    with evtRmnRPPS do
    begin
      Sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo  := '4564654'; Numero do recibo que será retificado.
        IndApuracao := iapuMensal;
        perApur := '2015-05';
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with ideTrabalhador do
      begin
        CpfTrab := '01234567890';
//        NisTrab := '09876543210';
//        qtdDepFP := 0;

        // os dados abaixo só devem ser informados em caso do processo existir
        // e houver decisão que incida sobre as  contribuições
//        procJudTrab.Clear;
//
//        with procJudTrab.New do
//        begin
//          tpTrib := tptPrevidenciaria;
//          nrProcJud := '95135703320156150258';
//          codSusp := '123456789';
//        end;
      end;

      dmDev.Clear;

      with dmDev.New do
      begin
        ideDmDev := '1';

        infoPerApur.ideEstab.Clear;

        with infoPerApur.IdeEstab.New do
        begin
          TpInsc := tiCNPJ;
          NrInsc := '012345678987654';

          remunPerApur.Clear;

          with remunPerApur.New do
          begin
            Matricula := 'A1234';
            CodCateg := 101;

            itensRemun.Clear;

            with itensRemun.New do
            begin
              CodRubr := '987654';
              ideTabRubr := 'E380';
              qtdRubr := 100;
              fatorRubr := 50;
              vrUnit := 3296.35;
              vrRubr := 3330.30;
            end;

            infoSaudeColet.detOper.Clear;

            with infoSaudeColet.detOper.New do
            begin
              cnpjOper := '01234567898765';
              regANS := 'A1B2C3';
              vrPgTit := 1.50;

              detPlano.Clear;

              with detPlano.New do
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

//        infoPerAnt.ideADC.Clear;
//
//        with infoPerAnt.ideADC.New do
//        begin
//          dtLei := Now;
//          nrLei := '321321/2017';
//          dtEf := Now;
//
//          idePeriodo.Clear;
//
//          with IdePeriodo.New do
//          begin
//            perRef := '2015-03';
//
//            ideEstab.Clear;
//
//            with IdeEstab.New do
//            begin
//              TpInsc := tiCNPJ;
//              NrInsc := '01234567898765';
//
//              remunPerAnt.Clear;
//
//              with remunPerAnt.New do
//              begin
//                Matricula := 'A1234';
//                CodCateg := 101;
//
//                itensRemun.Clear;
//
//                with itensRemun.New do
//                begin
//                  CodRubr := '987654';
//                  ideTabRubr := 'E380';
//                  qtdRubr := 100;
//                  fatorRubr := 50;
//                  vrUnit := 3296.35;
//                  vrRubr := 3330.30;
//                end;
//              end;
//            end;
//          end;
//        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1207;
begin
  with ACBreSocial1.Eventos.Periodicos.S1207.New do
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
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      ideBenef.cpfBenef := '88888888888';

      dmDev.Clear;

      with dmDev.New do
      begin
//        tpBenef := 01;
//        nrBenefic := '3132132';
        ideDmDev := '1';

//        itens.Clear;
//
//        with itens.New do
//        begin
//          CodRubr := '1';
//          ideTabRubr := 'E07';
//          vrRubr := 110.53;
//        end;
//
//        with itens.New do
//        begin
//          CodRubr := '2';
//          ideTabRubr := 'E08';
//          vrRubr := 2568.89;
//        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1210;
begin
  with ACBreSocial1.Eventos.Periodicos.S1210.New do
  begin
    with evtPgtos do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; - obrigatório se indRetif = ireRetificacao.
        IndApuracao := iapuMensal;
        perApur := '2015-05';
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with IdeBenef do
      begin
        cpfBenef := '01478523690';
        deps.vrDedDep := 100.50;

        InfoPgto.Clear;

        with InfoPgto.New do
        begin
          DtPgto := StrToDate('10/06/2015');
          tpPgto := tpPgtoRemun1200;
          IndResBr := tpNao;
          perRef := '2015-05';
          ideDmDev := '1';

          // -OS GRUPOS ABAIXO SÃO OPCIONAIS
          // grupo detPgtoFl agora é um collection
          detPgtoFl.Clear;

          with detPgtoFl.New do
          begin
            perRef := '2015-05';
            ideDmDev := '2';
            indPagtoTt := tpSim;
            vrLiq := 12365.43;
            nrRecArq := '132156156';

            retPagtoTot.Clear;

            with retPagtoTot.New do
            begin
              CodRubr := '1';
              ideTabRubr := '0';
              qtdRubr := 1.5;
              fatorRubr := 50;
              vrUnit := 100.10;
              vrRubr := 1001.00;

              penAlim.Clear;

              with penAlim.New do
              begin
                cpfBenef := '12345698745';
                dtNasctoBenef := Now;
                nmBenefic := 'Beneficiário da pensão';
                vlrPensao := 556.32;
              end;
            end;

            infoPgtoParc.Clear;

            with infoPgtoParc.New do
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

            with retPgtoTot.New do
            begin
              CodRubr := '321';
              ideTabRubr := '0';
              qtdRubr := 1.5;
              fatorRubr := 50.65;
              vrUnit := 500.85;
              vrRubr := 5001.65;
            end;

            infoPgtoParc.Clear;

            with infoPgtoParc.New do
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

          with detPgtoFer.New do
          begin
            CodCateg := 111;
            matricula := '54545';
            dtIniGoz := Now;
            qtDias := 30;
            vrLiq := 2500.32;

            detRubrFer.Clear;

            with detRubrFer.New do
            begin
              CodRubr := '888';
              ideTabRubr := '0';
              qtdRubr := 1;
              fatorRubr := 100;
              vrUnit := 144.33;
              vrRubr := 2500.32;

              penAlim.Clear;

              with penAlim.New do
              begin
                cpfBenef := '44455588899';
                dtNasctoBenef := Now;
                nmBenefic := 'Beneficiário de Pensão nas Férias';
                vlrPensao := 250.32;
              end;
            end;
          end;

          detPgtoAnt.Clear;

          with detPgtoAnt.New do
          begin
            CodCateg := 111;

            infoPgtoAnt.Clear;

            with infoPgtoAnt.New do
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

        if VersaoDFx >= veS01_02_00 then
        begin
          with InfoIRComplem do
          begin
            dtLaudo := StrToDate('10/06/2015');

            InfoDep.Clear;
            with InfoDep.New do
            begin
              cpfDep := '01234567890';
              dtNascto := StrToDate('10/06/2015');
              nome := 'Dependente';
              depIRRF := snfSim;
              tpDep := tdAgregadoOutros;
              descrDep := 'Descrição Agregado Outros';
            end;

            InfoIRCR.Clear;
            with InfoIRCR.New do
            begin
              tpCR := '056107';

              DedDepen.Clear;
              with DedDepen.New do
              begin
                tpRend := 1;
                cpfDep := '01234567890';
                vlrDedDep := 100;
              end;

              PenAlim.Clear;
              with PenAlim.New do
              begin
                tpRend := 1;
                cpfDep := '01234567890';
                vlrDedPenAlim := 100;
              end;

              PrevidCompl.Clear;
              with PrevidCompl.New do
              begin
                tpPrev := tprPrivada;
                cnpjEntidPC := '12345678000112';
                vlrDedPC := 100;
                vlrPatrocFunp := 100;
              end;

              InfoProcRet.Clear;
              with InfoProcRet.New do
              begin
                tpProcRet := tpprAdministrativo;
                nrProcRet := '12345678901234567890';
                codSusp := '12345678901234';

                InfoValores.Clear;
                with InfoValores.New do
                begin
                  indApuracao := iapuMensal;
                  vlrNRetido := 100;
                  vlrDepJud := 100;
                  vlrCmpAnoCal := 100;
                  vlrCmpAnoAnt := 100;
                  vlrRendSusp := 100;

                  DedSusp.Clear;
                  with DedSusp.New do
                  begin
                    indTpDeducao := tpdPrevidenciaOficial;
                    vlrDedSusp := 100;
                    cnpjEntidPC := '12345678000112';
                    vlrPatrocFunp := 100;

                    BenefPen.Clear;
                    with BenefPen.New do
                    begin
                      cpfDep := '12345678909';
                      vlrDepenSusp := 100;
                    end;
                  end;
                end;
              end;
            end;

            PlanSaude.Clear;
            with PlanSaude.New do
            begin
              cnpjOper := '12345678000112';
              regANS := '123456';
              vlrSaudeTit := 100;

              InfoDepSau.Clear;
              with InfoDepSau.New do
              begin
                cpfDep := '12345678901';
                vlrSaudeDep := 100;
              end;
            end;

            InfoReembMed.Clear;
            with InfoReembMed.New do
            begin
              indOrgReemb := '1';
              cnpjOper := '12345678000112';
              regANS := '123456';

              DetReembTit.Clear;
              with DetReembTit.New do
              begin
                tpInsc := tiCNPJ;
                nrInsc := '12345678000112';
                vlrReemb := 100;
                vlrReembAnt := 100;
              end;

              InfoReembDep.Clear;
              with InfoReembDep.New do
              begin
                cpfBenef := '12345678909';

                DetReembDep.Clear;
                with DetReembDep.New do
                begin
                  tpInsc := tiCNPJ;
                  nrInsc := '12345678000112';
                  vlrReemb := 100;
                  vlrReembAnt := 100;
                end;
              end;
            end;

          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1250;
begin
  with ACBreSocial1.Eventos.Periodicos.S1250.New do
  begin
    with EvtAqProd do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with InfoAquisProd.IdeEstabAdquir do
      begin
        tpInscAdq := tiCNPJ;
        nrInscAdq := '12345678910001';

        TpAquis.Clear;

        with TpAquis.New do
        begin
          indAquis := tpIdAquis(0);
          vlrTotAquis := 520000.80;

          IdeProdutor.Clear;

          with IdeProdutor.New do
          begin
            tpInscProd := tiCNPJ;
            nrInscProd := '98765432100015';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '64896549898789';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '648965498987894';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '648965498987894';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
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

            with InfoProcJud.New do
            begin
              nrProcJud := '56464897';
              codSusp := 333;
              vrCPNRet := 99999.99;
              vrRatNRet := 88888.88;
              vrSenarNRet := 77777.77;
            end;
          end;

          with IdeProdutor.New do
          begin
            tpInscProd := tiCNPJ;
            nrInscProd := '98765432100015';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '648965498987894';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '648965498987894';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '648965498987894';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
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

            with InfoProcJud.New do
            begin
              nrProcJud := '56464897';
              codSusp := 222;
              vrCPNRet := 99999.99;
              vrRatNRet := 88888.88;
              vrSenarNRet := 77777.77;
            end;
          end;

        end;

        with TpAquis.New do
        begin
          indAquis := tpIdAquis(1);
          vlrTotAquis := 520000.80;

          IdeProdutor.Clear;

          with IdeProdutor.New do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '74913476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '64896549898789';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
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

            with InfoProcJud.New do
            begin
              nrProcJud := '00000002';
              codSusp := 222;
              vrCPNRet := 22222.22;
              vrRatNRet := 22222.22;
              vrSenarNRet := 22222.22;
            end;
          end;

          with IdeProdutor.New do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '00003476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            InfoProcJud.Clear;

            with InfoProcJud.New do
            begin
              nrProcJud := '33333333';
              codSusp := 333;
              vrCPNRet := 33333.99;
              vrRatNRet := 33333.88;
              vrSenarNRet := 33333.77;
            end;
          end;
        end;

        with TpAquis.New do
        begin
          indAquis := tpIdAquis(2);
          vlrTotAquis := 33300.80;

          IdeProdutor.Clear;

          with IdeProdutor.New do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '74913476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            Nfs.Clear;

            with Nfs.New do
            begin
              serie := '00004';
              nrDocto := '64896549898789';
              dtEmisNF := Now;
              vlrBruto := 4000.54;
              vrCPDescPR := 3850.32;
              vrRatDescPR := 500.30;
              vrSenarDesc := 2500.30;
            end;

            with Nfs.New do
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

            with InfoProcJud.New do
            begin
              nrProcJud := '00000002';
              codSusp := 222;
              vrCPNRet := 22222.22;
              vrRatNRet := 22222.22;
              vrSenarNRet := 22222.22;
            end;
          end;

          with IdeProdutor.New do
          begin
            tpInscProd := tiCPF;
            nrInscProd := '00003476653';
            vlrBruto := 4000.54;
            vrCPDescPR := 3850.32;
            vrRatDescPR := 500.30;
            vrSenarDesc := 2500.30;

            InfoProcJud.Clear;

            with InfoProcJud.New do
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

procedure TfrmACBreSocial.GerareSocial1260;
begin
  with ACBreSocial1.Eventos.Periodicos.S1260.New do
  begin
    with EvtComProd do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with InfoComProd do
      begin
        with IdeEstabel do
        begin
          nrInscEstabRural := '123456789';

          TpComerc.Clear;

          with TpComerc.New do
          begin
            indComerc := tpIndComerc(0);
            vrTotCom := 5000.80;

            IdeAdquir.Clear;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;

              nfs.Clear;

              with Nfs.New do
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

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;

              nfs.Clear;

              with Nfs.New do
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

          with TpComerc.New do
          begin
            indComerc := tpIndComerc(1);
            vrTotCom := 5000.80;

            IdeAdquir.Clear;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;
            end;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
            end;

            with IdeAdquir.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '99999999999999';
              vrComerc := 8888.88;
              vrRetPR := 9999.99;

              nfs.Clear;

              with Nfs.New do
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

            with InfoProcJud.New do
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

procedure TfrmACBreSocial.GerareSocial1270;
begin
  with ACBreSocial1.Eventos.Periodicos.S1270.New do
  begin
    with EvtContratAvNP do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      remunAvNp.Clear;

      with RemunAvNP.New do
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

      with RemunAvNP.New do
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

      with RemunAvNP.New do
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

      with RemunAvNP.New do
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

procedure TfrmACBreSocial.GerareSocial1280;
begin
  with ACBreSocial1.Eventos.Periodicos.S1280.New do
  begin
    with EvtInfoComplPer do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      InfoSubstPatr.indSubstPatr := tpIndSubstPatr(0);
      InfoSubstPatr.percRedContrib := 500.20;

      InfoSubstPatrOpPort.Clear;

      with InfoSubstPatrOpPort.New do
      begin
        cnpjOpPortuario := '12345678900112';
      end;

      with InfoSubstPatrOpPort.New do
      begin
        cnpjOpPortuario := '98765432100014';
      end;

      // ClassTrib apenas informativo no S1280 para aplicação de regra de preenchimento da infoAtivConcom
      //   para Simples Nacional, preencher ClassTrib = ct03 em casos sem faturamento para gerar fatores zerados
      //ClassTrib := ct03;

      InfoAtivConcom.fatorMes := 9.00;
      InfoAtivConcom.fator13 := 1.00;

      with infoPercTransf11096 do
        percTransf := 5;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1298;
begin
  with ACBreSocial1.Eventos.Periodicos.S1298.New do
  begin
    with EvtReabreEvPer do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial1299;
begin
  with ACBreSocial1.Eventos.Periodicos.S1299.New do
  begin
    with EvtFechaEvPer do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
//        indRetif := tpIndRetificacao(0);
//        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

procedure TfrmACBreSocial.GerareSocial1300;
begin
  with ACBreSocial1.Eventos.Periodicos.S1300.New do
  begin
    with EvtContrSindPatr do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        IndApuracao := iapuMensal;
        perApur := '2015-06';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      ContribSind.Clear;

      with ContribSind.New do
      begin
        cnpjSindic := '01234567891111';
        vlrContribSind := 1500.50;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2190;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2190.New do
  begin
    with EvtAdmPrelim do
    begin
      Sequencial := 0;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      InfoRegPrelim.CpfTrab := '12345678901';
      InfoRegPrelim.DtNascto := Now - 9125;
      InfoRegPrelim.DtAdm := Now;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2200;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2200.New do
  begin
    with EvtAdmissao do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        // indRetif := tpIndRetificacao(1);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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
          codMunic := 1234567;
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
          CNH.ufCnh := 'SP';
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
            codMunic := 1234567;
            uf := 'SP';
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
          infoCota := snfNao;
          observacao := 'sem deficiencia';
        end;

        Dependente.Clear;

        with Dependente.New do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
          descrDep := 'Descrição da dependência';
        end;

        with Dependente.New do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
          descrDep := 'Descrição da dependência';
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
            matAnotJud := '1234567';

            FGTS.OpcFGTS := tpOpcFGTS(ofOptante);
            FGTS.DtOpcFGTS := date;

            with TrabTemporario do
            begin
              hipLeg := 1;
              justContr := 'teste';


              with IdeTomadorServ do
              begin
                TpInsc := tiCNPJ;
                NrInsc := '12345678901234';
                ideEstabVinc.TpInsc := tiCNPJ;
                ideEstabVinc.NrInsc := '12345678901234';
              end;

              ideEstabVinc.TpInsc := tiCNPJ;
              ideEstabVinc.NrInsc := '12345678901234';

              IdeTrabSubstituido.Clear;

              with IdeTrabSubstituido.New do
                CpfTrabSubst := '12345678912';
            end;

            aprend.indAprend := tiapContrDireta;
            aprend.cnpjEntQual := '12345678901234';
            aprend.TpInsc := tpTpInsc(1);
            aprend.NrInsc := '98765432109';
            aprend.cnpjPrat := '12345678901234';
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
            LocalTrabGeral.NrInsc := '12345678000112';
            LocalTrabGeral.DescComp := 'Descricao local geral teste';

            with LocalTrabDom do
            begin
              TpLograd    := '123';
              DscLograd   := 'LOCAL DOMESTICO';
              NrLograd    := '111';
              Complemento := 'Complemento';
              Bairro      := 'Bairro';
              Cep         := '85202630';
              CodMunic    := 1234567;
              Uf          := 'SP';
            end;
          end;

          with HorContratual do
          begin
            QtdHrsSem := 44;
            TpJornada := tpTpJornada(1);
            DscTpJorn := 'horario contratual';
            tmpParc := tpNaoeTempoParcial;

            horario.Clear;

            with horario.New do
            begin
              Dia := tpTpDia(diSegundaFeira);
              codHorContrat := '54';
            end;

            with horario.New do
            begin
              Dia := tpTpDia(diTercaFeira);
              codHorContrat := '10';
            end;
          end;

          FiliacaoSindical.Clear;

          with FiliacaoSindical.Add do
            CnpjSindTrab := '12345678901234';

          AlvaraJudicial.nrProcJud := '12345678901234567890';

          observacoes.Clear;

          with observacoes.New do
            observacao := 'Observacao';
        end;

        with SucessaoVinc do
        begin
          tpInscAnt := tiCNPJ;
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

procedure TfrmACBreSocial.GerareSocial2205;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2205.New do
  begin
    with EvtAltCadastral do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(1);
        //NrRecibo := '65.5454.987798798798';
        NrRecibo := '1.2.1234567890123456789';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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
        PaisNac := '123';

        with Nascimento do
        begin
          dtNascto   := StrToDate('10/02/1960');
          codMunic   := 3560025;
          uf         := 'SP';
          paisNascto := '123';
          paisNac    := '123';
          nmMae      := 'Joaquina';
          nmPai      := 'Sebastiao';
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
          CNH.ufCnh := 'SP';
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
            uf := 'SP';
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
          infoCota := snfNao;
          observacao := 'sem deficiencia';
        end;

        Dependente.Clear;

        with Dependente.New do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
          descrDep := 'Descrição da dependência';
        end;

        with Dependente.New do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          depSF := tpNao;
          incTrab := tpNao;
          descrDep := 'Descrição da dependência';
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

procedure TfrmACBreSocial.GerareSocial2206;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2206.New do
  begin
    with EvtAltContratual do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif = ireRetificacao;
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '96325874103';
      IdeVinculo.Matricula := 'A1234';

      with AltContratual do
      begin
        dtAlteracao := date;
        dtEf := Now;
        dscAlt := 'descrição da alteração';

        with Vinculo do
        begin
          TpRegTrab := trCLT;
          TpRegPrev := rpRGPS;

          with infoRegimeTrab do
          begin
            with InfoCeletista do
            begin
              TpRegJor := rjSubmetidosHorarioTrabalho;
              NatAtividade := navUrbano;
              dtBase := 08;
              cnpjSindCategProf := '15975395135700';

              trabTemporario.justProrr := 'Prorrogado porque eu quis';

              aprend.indAprend := tiapContrDireta;
              aprend.cnpjEntQual := '12345678901234';
              aprend.TpInsc := tpTpInsc(1);
              aprend.NrInsc := '98765432109';
              aprend.cnpjPrat := '12345678901234';
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
            nmFuncao := 'Função';

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
              dscJorn := 'Descrição Jornada';

              horario.Clear;

              with horario.New do
              begin
                Dia := diSegundaFeira;
                codHorContrat := '001';
              end;
            end;

            FiliacaoSindical.Clear;

            with FiliacaoSindical.Add do
              CnpjSindTrab := '12345678901234';

            AlvaraJudicial.nrProcJud := '12345678901234567890';

            // servPubl.mtvAlter := maPromocao;



          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2210;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2210.New do
  begin
    with EvtCAT do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '96325874103';
      IdeVinculo.Matricula := 'A1234';

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
        obsCAT := 'Teste';

        with LocalAcidente do
        begin
          tpLocal := tpTpLocal(1);
          dscLocal := 'Local Teste';
          codAmb := '1';
          tpLograd := 'R';
          DscLograd := 'Logradouro Teste';
          NrLograd := '111';
          bairro := 'centro';
          cep := '14800000';
          codMunic := 3512345;
          uf := 'SP';
          pais := '008';
          CodPostal := '6546';

          ideLocalAcid.TpInsc := tiCNPJ;
          ideLocalAcid.NrInsc := edtIdEmpregador.Text;
        end;

        ParteAtingida.Clear;

        with ParteAtingida.New do
        begin
          codParteAting := 753030000;
          lateralidade := tpLateralidade(1);
        end;

        with ParteAtingida.New do
        begin
          codParteAting := 753070700;
          lateralidade := tpLateralidade(2);
        end;

        with ParteAtingida.New do
        begin
          codParteAting := 753510200;
          lateralidade := tpLateralidade(3);
        end;

        AgenteCausador.Clear;

        with AgenteCausador.New do
          codAgntCausador := 302010300;

        with AgenteCausador.New do
          codAgntCausador := 302010600;

        with AgenteCausador.New do
          codAgntCausador := 302050500;

        with Atestado do
        begin
          codCNES := '1234567';
          dtAtendimento := Now;
          hrAtendimento := '1330';
          indInternacao := tpSim;
          durTrat := 5;
          indAfast := tpSim;
          dscLesao := 706090000;
          dscCompLesao := 'Descricao complementar';
          diagProvavel := 'Diagnostico teste';
          codCID := '1234';
          observacao := 'Observação teste';

          with Emitente do
          begin
            nmEmit := 'Emitente Teste';
            ideOC := idCRM;
            NrOc := '456123';
            ufOC := 'PR';
          end;
        end;

        CatOrigem.nrRecCatOrig := '123456';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2220;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2220.New do
  begin
    with evtMonit do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '5000';

      with exMedOcup do
      begin
        tpExameOcup := tpTpExameOcup(1);
        with Aso do
        begin
          DtAso := date;
          ResAso := tpResAso(1);

          Exame.Clear;

          with Exame.New do
          begin
            DtExm := date;
            procRealizado := '1234';
            obsProc := 'observação do procedimento realizado';
            ordExame := tpOrdExame(0);
            indResult := tpIndResult(1);
          end;

          with Exame.New do
          begin
            DtExm := date + 1;
            procRealizado := '4567';
            obsProc := 'observação do procedimento realizado';
            ordExame := tpOrdExame(0);
            indResult := tpIndResult(1);
          end;

          Medico.cpfMed := '12345678909';
          Medico.nisMed := '12345612345';
          Medico.NmMed  := 'TESTE DE MEDICO EXAMINADOR';
          Medico.nrCRM  := '6655666';
          Medico.ufCRM := 'SP';
        end;

        RespMonit.cpfResp := '12345678901';
        RespMonit.nmResp := 'NOME DO RESPONSAVEL';
        RespMonit.nrCRM := '666566';
        RespMonit.ufCRM := 'SP';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2221;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.NaoPeriodicos.S2221.New do
  begin
    with evtToxic do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi  := TpProcEmi(0);
        VerProc  := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '5000';

      with toxicologico do
      begin
        dtExame     := Date;
        cnpjLab     := '12548526587101';
        codSeqExame := '999999999';
        nmMed       := 'MEDICO TESTE';
        nrCRM       := '54646548';
        ufCRM       := 'SP';
//        indRecusa   := tpNao;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2230;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2230.New do
  begin
    with EvtAfastTemp do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        // NrRecibo := 'A.00.NNNNNNNNNNNNNNNNNNN'; Obrigatório se indRetif=ireRetificacao
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

          with infoAtestado.New do
          begin
            codCID := '1234';
            qtDiasAfast := 13;

            with Emitente do
            begin
              nmEmit := 'João das Neves';
              ideOC := idCRM;
              NrOc := '3690';
              ufOC := 'PR';
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
            ideOC := idCRM;
            NrOc := '12313';
            ufOC := 'SP';
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

procedure TfrmACBreSocial.GerareSocial2231;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2231.New do
  begin
    with evtCessao do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        nrRecibo := '65.5454.987798798798';
        procEmi := peAplicEmpregador;
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with ideVinculo do
      begin
        cpfTrab := '12345678901';
        matricula := 'A123';
      end;

      with infoCessao do
      begin

        with iniCessao do
        begin
          dtIniCessao := date;
          cnpjCess := '12345678901234';
          respRemun := tpSim;
        end;
{
        with fimCessao do
        begin
          dtTermCessao := date;
        end;
}
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2240;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2240.New do
  begin
    with EvtExpRisco do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        NrRecibo := '654654865656';
        ProcEmi := peAplicEmpregador;
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      IdeVinculo.CpfTrab := '12345678901';
      IdeVinculo.NisTrab := '12345678901';
      IdeVinculo.Matricula := '564545';

      with infoExpRisco do
      begin
        dtIniCondicao := date;

        InfoAmb.Clear;
        with InfoAmb.New do
        begin
          localAmb := laEstabProprioEmpregador;
          dscSetor := 'Descrição dscSetor';
          tpInsc := tiCNPJ;
          nrInsc := '12345678901234';
        end;
        with InfoAmb.New do
        begin
          localAmb := laEstabProprioEmpregador;
          dscSetor := 'Descrição dscSetor';
          tpInsc := tiCNPJ;
          nrInsc := '12345678901234';
        end;

        infoAtiv.dscAtivDes := 'DESCRICAO ATIVIDADE';

//        infoAtiv.ativPericInsal.Clear;
//        with infoAtiv.ativPericInsal.New do
//        begin
//          codAtiv := '06.540';
//        end;
//        with infoAtiv.ativPericInsal.New do
//        begin
//          codAtiv := '05.480';
//        end;

        agNoc.Clear;
        with agNoc.New do
        begin
          codAgNoc       := '12.12.123';
          tpAval         := tpaQualitativo;
          intConc        := 0.50;
          limTol         := 0.50;
          unMed          := 9;
          tecMedicao     := 'Tecnica de medicao';
//          insalubridade  := tpNao;
//          periculosidade := tpNao;
//          aposentEsp     := tpNao;
          nrProcJud      := '12345678901234567';

//          with epcEpi do
//          begin
//            utilizEPC := uEPCNaoAplica;
//            eficEpc   := tpNao;
//            utilizEPI := uEPIUtilizado;
//
//            epi.Clear;
//            with epi.New do
//            begin
//              caEPI         := '321654';
//              dscEPI        := 'CAPACETE';
//              eficEpi       := tpSim;
//              medProtecao   := tpSim;
//              condFuncto    := tpSim;
//              usoInint      := tpSim;
//              przValid      := tpSim;
//              periodicTroca := tpSim;
//              higienizacao  := tpSim;
//            end;
//          end;
        end;

        respReg.Clear;
        with respReg.New do
        begin
          cpfResp := '12345678901';
          NisResp := '12345678901';
          nmResp  := 'RESPONSAVEL';
          ideOC   := idOutros;
          dscOC   := 'ORGAO';
          NrOc    := '51561561';
          ufOC    := 'SP';
        end;

        obs.metErg   := 'METODOLOGIA';
        obs.obsCompl := 'OBS COMPLEMENTAR';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2245;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.NaoPeriodicos.S2245.New do
  begin
    with evtTreiCap do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := ireOriginal;
        NrRecibo := '654654865656';
        ProcEmi  := peAplicEmpregador;
        VerProc  := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      IdeVinculo.CpfTrab   := '12345678901';
      IdeVinculo.NisTrab   := '12345678901';
      IdeVinculo.Matricula := '564545';

      with treiCap do
      begin
        codTreiCap := '3203';
        obsTreiCap := 'observação'; 

        with infoComplem do
        begin
          dtTreiCap := date;
          durTreiCap := 15.5;
          modTreiCap := mtcPresencial;
          tpTreiCap  := ttcInicial;

          ideProfResp.Clear;

          with ideProfResp.New do
          begin
            cpfProf  := '12345678901';
            nmProf   := 'PROFISSIONAL';
            tpProf   := tpTpProf(1);
            formProf := 'ENGENHEIRO';
            codCBO   := '515661';
            nacProf  := tpNacProf(1);
          end;
        end; 
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2250;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

  with ACBreSocial1.Eventos.NaoPeriodicos.S2250.New do
  begin
    with EvtAvPrevio do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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
          DetAvPrevio.observacao := 'Observacao aviso previo';
        end
        else // cancelamento aviso
        begin
          CancAvPrevio.dtCancAvPrv := Now;
          CancAvPrevio.observacao := 'Observacao cancelamento aviso previo';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2260;
begin
  if VersaoDFx > ve02_05_00 then
    exit;

 with ACBreSocial1.Eventos.NaoPeriodicos.S2260.New do
  begin
    with EvtConvInterm do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
//        NrRecibo := '65.5454.987798798798';
        ProcEmi  := TpProcEmi(0);
        VerProc  := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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
            UF          := 'SP';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2298;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2298.New do
  begin
    with EvtReintegr do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

procedure TfrmACBreSocial.GerareSocial2299;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2299.New do
  begin
    with EvtDeslig do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

        indPDV := snfSim;

        indCumprParc := cpaCumprimentoTotal;

        // Obsercação opcional - versão 2.04.01
        observacao := 'Anotações relevantes sobre o desligamento que não tenham campo próprio';

        // Obsercação opcional - versão 2.04.02
        with observacoes.New do
         observacao := 'Anotações relevantes sobre o desligamento que não tenham campo próprio';

        SucessaoVinc.tpInscSuc := tiCNPJ;
        SucessaoVinc.cnpjSucessora := '12345678912345';

        with VerbasResc do
        begin
          dmDev.Clear;

          with dmDev.New do
          begin
            ideDmDev := '1234567890';

            with infoPerApur do
            begin
              ideEstabLot.Clear;

              with ideEstabLot.New do
              begin
                TpInsc := tiCNPJ;
                NrInsc := '12345678901234';
                codLotacao := 'A1234';

                detVerbas.Clear;

                with detVerbas.New do
                begin
                  CodRubr := 'Pg123';
                  ideTabRubr := 'A01';
                  qtdRubr := 2;
                  fatorRubr := 200.65;
                  vrUnit := 152.35;
                  vrRubr := 304.70;
                end;

                with infoSaudeColet.detOper.New do
                begin
                  cnpjOper := '74563215000195';
                  regANS := '123456';
                  vrPgTit := 150.65;

                  with detPlano.New do
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

              with ideADC.New do
              begin
                dtAcConv := Now;
                tpAcConv := tacAcordoColTrab;
                dtEfAcConv := Now;
                dsc := 'Detalhamento';

                idePeriodo.Clear;

                with IdePeriodo.New do
                begin
                  perRef := '2017-05';

                  ideEstabLot.Clear;

                  with ideEstabLot.New do
                  begin
                    TpInsc := tiCNPJ;
                    NrInsc := '12345678901234';
                    codLotacao := 'A1234';

                    detVerbas.Clear;

                    with detVerbas.New do
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

          with procJudTrab.New do
          begin
            tpTrib := tptIRRF;
            nrProcJud := '01236547890123654789';
            codSusp := '1235';
          end;

          with infoMV do
          begin
            indMV := tpIndMV(0);

            remunOutrEmpr.Clear;

            with remunOutrEmpr.New do
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

        with consigFGTS.New do
        begin
          idConsig := tpSim;
          insConsig := '12345';
          nrContr := '123456';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2300;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2300.New do
  begin
    with EvtTSVInicio do
    begin
      Sequencial := 0;

      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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
          CNH.ufCnh := 'SP';
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
          Brasil.uf := 'SP';

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

        with Dependente.New do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          DtNascto := date;
          cpfDep := '99999999909';
          depIRRF := tpSim;
          depSF := tpNao;
          descrDep := 'Descrição da dependência';
        end;

        with Dependente.New do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '99999999909';
          depIRRF := tpSim;
          depSF := tpNao;
          descrDep := 'Descrição da dependência';
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
              uf := 'PR';
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
              uf := 'SP';
            end;

            supervisorEstagio.cpfSupervisor := '88888888801';
            supervisorEstagio.nmSuperv := 'Pedro das Pedras';
          end;

          LocalTrabGeral.TpInsc := tiCNPJ;
          LocalTrabGeral.NrInsc := '12345678000112';
          LocalTrabGeral.DescComp := 'Descricao local geral teste';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2306;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2306.New do
  begin
    with EvtTSVAltContr do
    begin
      Sequencial := 0;

      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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
              uf := 'PR';
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
              uf := 'SP';
            end;

            supervisorEstagio.cpfSupervisor := '12345678901';
            supervisorEstagio.nmSuperv := 'Pedro das Pedras';
          end;

          LocalTrabGeral.TpInsc := tiCNPJ;
          LocalTrabGeral.NrInsc := '12345678000112';
          LocalTrabGeral.DescComp := 'Descricao local geral teste';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2399;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2399.New do
  begin
    with EvtTSVTermino do
    begin
      Sequencial := 0;

      with IdeEvento do
      begin
        indRetif := tpIndRetificacao(0);
        NrRecibo := '65.5454.987798798798';
        ProcEmi := TpProcEmi(0);
        VerProc := '1.0';
      end;

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

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

          with dmDev.New do
          begin
            ideDmDev := '012345';

            ideEstabLot.Clear;

            with ideEstabLot.New do
            begin
              TpInsc := tiCNPJ;
              NrInsc := '12345678987654';
              codLotacao := 'A1234';

              detVerbas.Clear;

              with detVerbas.New do
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

                with detOper.New do
                begin
                  cnpjOper := '89652048000195';
                  regANS := '123456';
                  vrPgTit := 1500.65;

                  detPlano.Clear;

                  with detPlano.New do
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

          with procJudTrab.New do
          begin
            tpTrib := tpTpTributo(0);
            nrProcJud := '123456789';
            codSusp := '123456';
          end;

          with infoMV do
          begin
            indMV := tpIndMV(0);

            with remunOutrEmpr.New do
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

procedure TfrmACBreSocial.GerareSocial2400;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2400.New do
  begin
    with evtCdBenefIn do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        nrRecibo := '65.5454.987798798798';
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with beneficiario do
      begin
        cpfBenef := '12345678910';
        nmBenefic := 'Nome do beneficiario';
        dtNascto := date;
        dtInicio := date;
        sexo := 'F';
        racaCor := 2;
        estCiv := 1;
        incFisMen := tpNao;
//        dtIncFisMen := date;

        with endereco do
        begin
          with brasil do
          begin
            tpLograd := 'R';
            dscLograd := 'Rua Parmenides';
            nrLograd := '123456';
            complemento := 'fundos';
            bairro := 'Jd Filosofia';
            cep := '88888888';
            codMunic := 4141414;
            uf := 'SP';
          end;
{
          with exterior do
          begin
            paisResid := '063';
            dscLograd := 'St. Abbey Road';
            nrLograd := '123456';
            complemento := 'apto 010';
            bairro := 'RubberSoul';
            nmCid := 'Buenos Aires';
            codPostal := '987654';
          end;
}
        end;

        dependente.Clear;

        with dependente.New do
        begin
          tpDep := tdConjuge;
          nmDep := 'Dependente 1';
          dtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          sexoDep := 'F';
          depSF := tpNao;
          incTrab := tpNao;
          descrDep := 'Descrição da dependência';
        end;

        with Dependente.New do
        begin
          tpDep := tdFilhoOuEnteado;
          nmDep := 'Dependente 2';
          DtNascto := date;
          cpfDep := '12345678901';
          depIRRF := tpSim;
          sexoDep := 'M';
          depSF := tpNao;
          incTrab := tpNao;
          descrDep := 'Descrição da dependência';
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2405;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2405.New do
  begin
    with evtCdBenefAlt do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        nrRecibo := '65.5454.987798798798';
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with ideBenef do
      begin
        cpfBenef := '12345678901';
      end;

      with Alteracao do
      begin
        dtAlteracao := date;

        with dadosBenef do
        begin
          nmBenefic := 'Nome do beneficiario';
          sexo := 'M';
          racaCor := 1;
          estCiv := 1;
          incFisMen := tpNao;

          with endereco do
          begin
            with brasil do
            begin
              tpLograd := 'R';
              dscLograd := 'Rua Parmenides';
              nrLograd := '123456';
              complemento := 'fundos';
              bairro := 'Jd Filosofia';
              cep := '88888888';
              codMunic := 4141414;
              uf := 'SP';
            end;
{
            with exterior do
            begin
              paisResid := '063';
              dscLograd := 'St. Abbey Road';
              nrLograd := '123456';
              complemento := 'apto 010';
              bairro := 'RubberSoul';
              nmCid := 'Buenos Aires';
              codPostal := '987654';
            end;
}
            dependente.Clear;

            with dependente.New do
            begin
              tpDep := tdConjuge;
              nmDep := 'Dependente 1';
              dtNascto := date;
              cpfDep := '12345678901';
              //depIRRF := tpSim;
              sexoDep := 'F';
              depSF := tpNao;
              incTrab := tpNao;
              descrDep := 'Descrição da dependência';
            end;

            with Dependente.New do
            begin
              tpDep := tdFilhoOuEnteado;
              nmDep := 'Dependente 2';
              DtNascto := date;
              cpfDep := '12345678901';
              //depIRRF := tpSim;
              sexoDep := 'M';
              depSF := tpNao;
              incTrab := tpNao;
              descrDep := 'Descrição da dependência';
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2410;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2410.New do
  begin
    with evtCdBenIn do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        nrRecibo := '65.5454.987798798798';
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with beneficiario do
      begin
        cpfBenef := '12345678901';
        matricula := 'A1234';
        cnpjOrigem := '12345678901234';
      end;

      with infoBenInicio do
      begin
        cadIni := tpSim;
        indSitBenef := tpIndSitBenef(1);
        nrBeneficio := '12345678901234567890';
        dtIniBeneficio := date;
        dtPublic := date;

        with dadosBeneficio do
        begin
          tpBeneficio := 101;
          tpPlanRP := prpSemSegregacaoDaMassa;
          dsc := 'Descrição do benefício';
          indDecJud := tpSimNaoFacultativo(2);

          with infoPenMorte do
          begin
            tpPenMorte := pmVitalicia;

            with instPenMorte do
            begin
              cpfInst := '12345678901';
              dtInst := date;
            end;
          end;
{
          with sucessaoBenef do
          begin
            cnpjOrgaoAnt := '12345678901234';
            nrBeneficioAnt := '12345678901234567890';
            dtTransf := date;
            observacao := 'Qualquer observação';
          end;

          with mudancaCPF do
          begin
            cpfAnt := '12345678901';
            nrBeneficioAnt := '12345678901234567890';
            dtAltCPF := date;
            observacao := 'Qualquer observação';
          end;

          with infoBenTermino do
          begin
            dtTermBeneficio := date;
            mtvTermino := tmcbTerminoDoPrazoDoBeneficio;
          end;
}
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2416;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2416.New do
  begin
    with evtCdBenAlt do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        nrRecibo := '65.5454.987798798798';
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with ideBeneficio do
      begin
        cpfBenef := '12345678901';
        nrBeneficio := '12345678901234567890';
      end;

      with infoBenAlteracao do
      begin
        dtAltBeneficio := date;

        with dadosBeneficio do
        begin
          tpBeneficio := 101;
          tpPlanRP := prpMantidoPeloTesouro;
          dsc := 'Descrição do benefício';
          indSuspensao := tpNao;

          with infoPenMorte do
          begin
            tpPenMorte := pmVitalicia;
          end;

          with suspensao do
          begin
            mtvSuspensao := mtvSuspensaoPorNaoRecadastramento;
            dscSuspensao := 'Motivo da suspensão';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2418;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2418.New do
  begin
    with evtReativBen do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        nrRecibo := '65.5454.987798798798';
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with ideBeneficio do
      begin
        cpfBenef := '12345678901';
        nrBeneficio := '12345678901234567890';
      end;

      with infoReativ do
      begin
        dtEfetReativ := date;
        dtEfeito := date;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2420;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S2420.New do
  begin
    with evtCdBenTerm do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := tpIndRetificacao(0);
        nrRecibo := '65.5454.987798798798';
        procEmi := TpProcEmi(0);
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with ideBeneficio do
      begin
        cpfBenef := '12345678901';
        nrBeneficio := '12345678901234567890';
      end;

      with infoBenTermino do
      begin
        dtTermBeneficio := date;
        mtvTermino := tmcbReversao;
        cnpjOrgaoSuc := '12345678901234';
        novoCPF := '12345678901';
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2500;
begin
  if VersaoDFx < veS01_01_00 then
    exit;

  with ACBreSocial1.Eventos.NaoPeriodicos.S2500.New do
  begin
    with evtProcTrab do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        // nrRecibo := '1.2.0000000000000000000';
        procEmi := peAplicEmpregador;
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;

        with ideResp do
        begin
          tpInsc := tiCPF;
          nrInsc := '01234567890';
        end;
      end;

      with infoProcesso do
      begin
        origem := oprProcessoJudicial;
        nrProcTrab := '01234567890123456789';
        obsProcTrab := 'Alguma observação';

        with dadosCompl do
        begin
          with infoProcJud do
          begin
            dtSent := Now;
            ufVara := 'RJ';
            codMunic := 3304557;
            idVara := 365;
          end;
{
          with infoCCP do
          begin
            dtCCP := Now;
            tpCCP := CCPAmbitoSindicato;
            cnpjCCP := '01234567890123';
          end;
}
        end;
      end;

      with ideTrab do
      begin
        cpfTrab := '01234567890';
        nmTrab := 'Trabalhador Um';
        dtNascto := Now - 9501;

        with dependente.New do
        begin
          cpfDep := '01234567891';
          tpDep := tdConjuge;
          descDep := 'Descrever o dep set tpDep = 99';
        end;

        with infoContr.New do
        begin
          tpContr := TrabalhadorComVinculoFormalizadoSemAlteracaoNasDatasDeAdmissaoEDeDesligamento;
          indContr := tpSim;
          dtAdmOrig := Now;
          indReint := snfNao;
          indCateg := tpNao;
          indNatAtiv := tpSim;
          indMotDeslig := tpNao;
          indUnic := snfSim;
          matricula := '3515209';
          codCateg := 101;
          dtInicio := Now;

          with infoCompl do
          begin
            codCBO := '012345';
            natAtividade := navUrbano;

            with remuneracao.New do
            begin
              dtRemun := Now - 3250;
              vrSalFx := 3280.25;
              undSalFixo := sfPorMes;
              dscSalVar := 'Descrição do salário se for por tarefa ou não aplicável';
            end;

            with infoVinc do
            begin
              tpRegTrab := trCLT;
              tpRegPrev := rpRGPS;
              dtAdm := Now - 4000;
              tmpParc := tpNaoeTempoParcial;

              with duracao do
              begin
                tpContr := PrazoDeterminado;
                dtTerm := Now + 365;
                clauAssec := tpNao;
                objDet := 'Descrever o objeto determinante para o prazo determinado';
              end;

              with observacoes.New do
                observacao := 'Observação 1';

              with observacoes.New do
                observacao := 'Observação 2';

              with sucessaoVinc do
              begin
                tpInsc := tiCNPJ;
                nrInsc := '01234567890123';
                matricAnt := '338560-22';
                dtTransf := Now - 3200;
              end;

              with infoDeslig do
              begin
                dtDeslig := Now + 365;
                mtvDeslig := '99';
                dtProjFimAPI := Now + 395;
              end;
            end;

            with infoTerm do
            begin
              dtTerm := Now + 365;
              mtvDesligTSV := '01';
            end;
          end;

          with mudCategAtiv.New do
          begin
            codCateg := 101;
            natAtividade := navUrbano;
            dtMudCategAtiv := Now + 165;
          end;

          with unicContr.New do
          begin
            matUnic := '3657829-001';
            codCateg := 101;
            dtInicio := Now;
          end;

          with ideEstab do
          begin
            tpInsc := tiCNPJ;
            nrInsc := '01234567890123';

            with infoVlr do
            begin
              compIni := '2012-07';
              compFim := '2022-05';
              repercProc := repDecisaoComPagamentoDeVerbasDeNaturezaRemuneratoria;
              vrRemun := 6785.22;
              vrAPI := 6785.22;
              vr13API := 2252.32;
              vrInden := 3250.55;
              vrBaseIndenFGTS := 6785.22;
              pagDiretoResc := snfNao;

              with idePeriodo.New do
              begin
                perRef := '2012-07';

                with baseCalculo do
                begin
                  vrBcCpMensal := 0;
                  vrBcCp13 := 10000;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial2501;
begin
  if VersaoDFx < veS01_01_00 then
    exit;

  with ACBreSocial1.Eventos.NaoPeriodicos.S2501.New do
  begin
    with evtContProc do
    begin
      sequencial := 0;

      with ideEvento do
      begin
        indRetif := ireOriginal;
        // nrRecibo := '1.2.0000000000000000000';
        procEmi := peAplicEmpregador;
        verProc := '1.0';
      end;

      with ideEmpregador do
      begin
        tpInsc := tiCNPJ;
        nrInsc := edtIdEmpregador.Text;
      end;

      with ideProc do
      begin
        nrProcTrab := '012345678901234';
        perApurPgto := '2022-09';
        obs := 'Qualquer observação necessária';
      end;

      with ideTrab.New do
      begin
        cpfTrab := '01234567890';

        with calcTrib.New do
        begin
          perRef := '2022-08';
          vrBcCpMensal := 1212;
          vrBcCp13 := 1212;

          with infoCRContrib.New do
          begin
            tpCR := '113851';
            vrCR := 242.40;
          end;
        end;

        with infoCRIRRF.New do
        begin
          tpCR := '593656';
          vrCR := 76;

          with infoIR.New do
          begin
            vrRendTrib := 3875.22;
            vrRendTrib13 := 976.40;
            vrRendMoleGrave := 2856.32;
            vrRendIsen65 := 1525.32;
            vrJurosMora := 32.00;
            vrRendIsenNTrib := 3284.33;
            descIsenNTrib := 'Descrição de isento ou não tributável';
            vrPrevOficial := 685.22;
          end;

          with infoRRA do
          begin
            descRRA := 'Descrição dos rendimentos acumulados';
            qtdMesesRRA := 15.5;

            with despProcJud do
            begin
              vlrDespCustas := 1500.85;
              vlrDespAdvogados := 685.95;
            end;

            with ideAdv.New do
            begin
              tpInsc := tiCPF;
              nrInsc := '12345678901';
              vlrAdv := 685.95;
            end;
          end;

          with dedDepen.New do
          begin
            tpRend := 11;
            cpfDep := '12345678901';
            vlrDeducao := 294;
          end;

          with penAlim.New do
          begin
            tpRend := 11;
            cpfDep := '12345678901';
            vlrPensao := 1865.45;
          end;

          with infoProcRet.New do
          begin
            tpProcRet := tpprAdministrativo;
            nrProcRet := '12345678901234567';
            codSusp := '1234567890';

            with infoValores.New do
            begin
              indApuracao := iapuMensal;
              vlrNRetido := 4987.22;
              vlrDepJud := 908.32;
              vlrCmpAnoCal := 325.99;
              vlrCmpAnoAnt := 295.00;
              vlrRendSusp := 988.24;

              with dedSusp.New do
              begin
                indTpDeducao := tpdtPrevidenciaOficial;
                vlrDedSusp := 654.99;

                with benefPen.New do
                begin
                  cpfDep := '12345678901';
                  vlrDepenSusp := 185.65;
                end;
              end;
            end;
          end;
        end;

        with infoIRComplem do
        begin
          dtLaudo := Date;

          with infoDep.New do
          begin
            cpfDep := '12345678901';
            dtNascto := Date;
            nome := 'Maria Antonieta da Silva e Souza';
            depIRRF := snfSim;
            tpDep := tdConjuge;
            descrDep := 'Alguma descrição do dependente';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBreSocial.GerareSocial3000;
begin
  with ACBreSocial1.Eventos.NaoPeriodicos.S3000.New do
  begin
    with EvtExclusao do
    begin
      Sequencial := 0;

      IdeEvento.ProcEmi := TpProcEmi(0);
      IdeEvento.VerProc := '1.0';

      IdeEmpregador.TpInsc := tiCNPJ;
      IdeEmpregador.NrInsc := edtIdEmpregador.Text;

      with InfoExclusao do
      begin
        tpEvento := TTipoEvento(teS2100);
        nrRecEvt := '12345789987654321';

        ideTrabalhador.CpfTrab := '12345678950';
        ideTrabalhador.NisTrab := '12345678901';

        IdeFolhaPagto.IndApuracao := iapuMensal;
        IdeFolhaPagto.perApur := '2015-05';
      end;
    end;
  end;
end;

function TfrmACBreSocial.GetTipoOperacao: TModoLancamento;
begin
  case rdgOperacao.ItemIndex of
    1: Result := mlAlteracao;
    2: Result := mlExclusao;
  else
    Result := mlInclusao;
  end;
end;

procedure TfrmACBreSocial.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBreSocial1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBreSocial1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBreSocial1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBreSocial1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBreSocial.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertCNPJ);
end;

procedure TfrmACBreSocial.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBreSocial1.SSL.CertDataVenc));
end;

procedure TfrmACBreSocial.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBreSocial1.SSL.UseCertificateHTTP;
  ACBreSocial1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBreSocial1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBreSocial1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBreSocial.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBreSocial1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBreSocial1.SSL.CertCertificadora);
end;

procedure TfrmACBreSocial.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBreSocial1.SSL do
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

procedure TfrmACBreSocial.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertNumeroSerie);
end;

procedure TfrmACBreSocial.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBreSocial.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBreSocial1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBreSocial.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBreSocial1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBreSocial1.SSL.CertRazaoSocial);
end;

procedure TfrmACBreSocial.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBreSocial.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBreSocial.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBreSocial.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBreSocial1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBreSocial.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBreSocial.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoeSocial;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  L: TEmpregador;
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
  for K := Low(TVersaoeSocial) to High(TVersaoeSocial) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoeSocial), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  cbTEmpregador.Items.Clear;
  for L := Low(TEmpregador) to High(TEmpregador) do
    cbTEmpregador.Items.Add(GetEnumName(TypeInfo(TEmpregador), Integer(L)));
  cbTEmpregador.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBreSocial.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
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
    Ini.WriteString( 'Geral', 'IdEmpregador',     edtIdEmpregador.Text);
    Ini.WriteString( 'Geral', 'IdTransmissor',    edtIdTransmissor.Text);
    Ini.WriteInteger('Geral', 'TipoEmpregador',   cbTEmpregador.ItemIndex);

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

    Ini.WriteBool(  'Arquivos', 'Salvar',             cbxSalvarArqs.Checked);
    Ini.WriteBool(  'Arquivos', 'PastaMensal',        cbxPastaMensal.Checked);
    Ini.WriteBool(  'Arquivos', 'AddLiteral',         cbxAdicionaLiteral.Checked);
    Ini.WriteBool(  'Arquivos', 'EmissaoPatheSocial', cbxEmissaoPatheSocial.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento',   cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',     cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo',   cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PatheSocial',        edtPatheSocial.Text);

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBreSocial.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBreSocial.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBreSocial.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBreSocial.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBreSocial.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBreSocial.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBreSocial.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
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
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoeSocial), integer(cbVersaoDF.ItemIndex) ));

    edtIdEmpregador.Text    := Ini.ReadString( 'Geral', 'IdEmpregador',   '');
    edtIdTransmissor.Text   := Ini.ReadString( 'Geral', 'IdTransmissor',  '');
    cbTEmpregador.ItemIndex := Ini.ReadInteger('Geral', 'TipoEmpregador', 0);

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

    cbxSalvarArqs.Checked         := Ini.ReadBool(  'Arquivos', 'Salvar',             false);
    cbxPastaMensal.Checked        := Ini.ReadBool(  'Arquivos', 'PastaMensal',        false);
    cbxAdicionaLiteral.Checked    := Ini.ReadBool(  'Arquivos', 'AddLiteral',         false);
    cbxEmissaoPatheSocial.Checked := Ini.ReadBool(  'Arquivos', 'EmissaoPatheSocial', false);
    cbxSalvaPathEvento.Checked    := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento',   false);
    cbxSepararPorCNPJ.Checked     := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',     false);
    cbxSepararPorModelo.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorModelo',   false);
    edtPatheSocial.Text           := Ini.ReadString('Arquivos', 'PatheSocial',        '');

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBreSocial.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBreSocial1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBreSocial1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBreSocial1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBreSocial1.SSL.DescarregarCertificado;

  with ACBreSocial1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoeSocial(cbVersaoDF.ItemIndex);

    IdEmpregador   := edtIdEmpregador.Text;
    IdTransmissor  := edtIdTransmissor.Text;
    TipoEmpregador := TEmpregador(cbTEmpregador.ItemIndex);
  end;

  with ACBreSocial1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBreSocial1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBreSocial1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBreSocial1.Configuracoes.Arquivos do
  begin
    Salvar             := cbxSalvarArqs.Checked;
    SepararPorMes      := cbxPastaMensal.Checked;
    AdicionarLiteral   := cbxAdicionaLiteral.Checked;
    EmissaoPatheSocial := cbxEmissaoPatheSocial.Checked;
    SepararPorCNPJ     := cbxSepararPorCNPJ.Checked;
    SepararPorModelo   := cbxSepararPorModelo.Checked;
    PathSchemas        := edtPathSchemas.Text;
    PatheSocial        := edtPatheSocial.Text;
    PathMensal         := GetPatheSocial(0);
    PathSalvar         := PathMensal;
  end;
end;

procedure TfrmACBreSocial.PathClick(Sender: TObject);
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

procedure TfrmACBreSocial.sbPatheSocialClick(Sender: TObject);
begin
  PathClick(edtPatheSocial);
end;

procedure TfrmACBreSocial.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBreSocial.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBreSocial1.SSL.SelecionarCertificado;
end;

procedure TfrmACBreSocial.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
//  ASerie: String;
  AddRow: Boolean;
begin
  ACBreSocial1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBreSocial1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBreSocial1.SSL.ListaCertificados[I] do
    begin
//      ASerie := NumeroSerie;

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

procedure TfrmACBreSocial.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBreSocial.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TfrmACBreSocial.btnGerarClick(Sender: TObject);
var
  i: Integer;
begin
  SelecionaEventos;

  ACBreSocial1.Eventos.Gerar;   // Somente Gera os XMLs dos Eventos
  ACBreSocial1.Eventos.Assinar; // Somente Assina os XMLs
  ACBreSocial1.Eventos.Validar; // Somente Valida os XMLs

//  ACBreSocial1.Eventos.GerarXMLs; // Gera, Assina e Valida os XMLs dos Eventos

  ACBreSocial1.Eventos.SaveToFiles; // Salva os XMLs em Disco

  memoLog.Lines.Add('XML de Eventos Gerados, Assinados e Validados com Sucesso!');
  memoLog.Lines.Add(' ');

  for i := 0 to ACBreSocial1.Eventos.Gerados.Count -1 do
  begin
    MemoLog.Lines.Add('Tipo Evento.: ' + TipoEventoToStr(ACBreSocial1.Eventos.Gerados.Items[i].TipoEvento));
    MemoLog.Lines.Add('ID do Evento: ' + ACBreSocial1.Eventos.Gerados.Items[i].idEvento);
    MemoLog.Lines.Add('Evento Salvo: ' + ACBreSocial1.Eventos.Gerados.Items[i].PathNome);
  end;

  pgRespostas.ActivePageIndex := 3;
end;

procedure TfrmACBreSocial.btnCarregarXMLClick(Sender: TObject);
var
  i: Integer;
  LidoXML: Boolean;
begin
  repeat
    OpenDialog1.Title := 'Selecione o Evento (Arquivo XML)';
    OpenDialog1.DefaultExt := '*.xml';
    OpenDialog1.Filter :=
      'Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
    OpenDialog1.InitialDir := ACBreSocial1.Configuracoes.Arquivos.PathSalvar;

    LidoXML := OpenDialog1.Execute;

    if LidoXML then
      ACBreSocial1.Eventos.LoadFromFile(OpenDialog1.FileName);
  until not LidoXML;

  MemoResp.Clear;
  MemoResp.Lines.Clear;
  MemoResp.Lines.Add('XML de Eventos Carregado com Sucesso!');
  MemoResp.Lines.Add(' ');

  for i := 0 to ACBreSocial1.Eventos.Gerados.Count -1 do
  begin
    MemoResp.Lines.Add('Tipo Evento.: ' + TipoEventoToStr(ACBreSocial1.Eventos.Gerados.Items[i].TipoEvento));
    MemoResp.Lines.Add('Evento Salvo: ' + ACBreSocial1.Eventos.Gerados.Items[i].PathNome);
  end;

  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBreSocial.btnCarregarINIClick(Sender: TObject);
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

//  ACBreSocial1.Eventos.Gerar;   // Somente Gera os XMLs dos Eventos
  ACBreSocial1.Eventos.Assinar; // Somente Assina os XMLs
  ACBreSocial1.Eventos.Validar; // Somente Valida os XMLs
  ACBreSocial1.Eventos.SaveToFiles; // Salva os XMLs em Disco

  MemoResp.Clear;
  MemoResp.Lines.Clear;
  MemoResp.Lines.Add('INI de Eventos Carregado com Sucesso!');
  MemoResp.Lines.Add(' ');

  for I := 0 to ACBreSocial1.Eventos.Gerados.Count -1 do
  begin
    MemoResp.Lines.Add('Tipo Evento.: ' + TipoEventoToStr(ACBreSocial1.Eventos.Gerados.Items[i].TipoEvento));
    MemoResp.Lines.Add('Evento Salvo: ' + ACBreSocial1.Eventos.Gerados.Items[i].PathNome);
  end;

  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBreSocial.btnEnviarClick(Sender: TObject);
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
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.EnvioLote.PathNome);

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

    pgRespostas.ActivePageIndex := 3;
  finally
    ACBreSocial1.Eventos.Clear;
  end;
end;

procedure TfrmACBreSocial.btnGerarEnviarClick(Sender: TObject);
var
  i: Integer;
begin
  if chkClear.Checked then
    LimparDocsPasta;

  try
    SelecionaEventos;
    ACBreSocial1.AssinarEventos;

    ACBreSocial1.Enviar(TESocialGrupo(rdgGrupo.ItemIndex + 1));

    MemoResp.Lines.Text := ACBreSocial1.WebServices.EnvioLote.RetWS;

    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.EnvioLote.RetEnvioLote do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.EnvioLote.PathNome);

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

    pgRespostas.ActivePageIndex := 3;
  finally
    ACBreSocial1.Eventos.Clear;
  end;
end;

procedure TfrmACBreSocial.btnConsultarClick(Sender: TObject);
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
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.ConsultaLote.PathNome);

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
            Add(' - ID Evento..........: ' + retEventos.Items[i].Id);
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

    pgRespostas.ActivePageIndex := 3;
  end;
end;

procedure TfrmACBreSocial.btnConsIdeEveEmpClick(Sender: TObject);
var
  TipoEvento, Periodo: string;
  i: Integer;
  Ok: Boolean;
begin
  TipoEvento := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Empregador', 'Tipo do Evento no formato S-xxxx', TipoEvento)) then
    Exit;

  Periodo := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Empregador', 'Perido de apuracao no formato 01/MM/AAAA', Periodo)) then
    Exit;

  if ACBreSocial1.ConsultaIdentificadoresEventosEmpregador(edtIdEmpregador.Text,
                       StrToTipoEvento(Ok, TipoEvento), StrToDate(Periodo)) then
  begin

    MemoResp.Lines.Text := ACBreSocial1.WebServices.ConsultaIdentEventos.RetWS;

    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.ConsultaIdentEventos.PathNome);
        Add('');
        Add('Qtde Total de Eventos na Consulta: ' + IntToStr(RetIdentEvts.qtdeTotEvtsConsulta));
        Add('dhUltimo Evento Retornado: ' + DateTimeToStr(RetIdentEvts.dhUltimoEvtRetornado));

        for i := 0 to RetIdentEvts.Count - 1 do
        begin
          Add('Identificação do Evento:');
          Add('');
          Add(' - ID Evento: ' + RetIdentEvts.Items[i].Id);
          Add(' - Nr Recibo: ' + RetIdentEvts.Items[i].nrRec);
        end;
      end;
    end;
    pgRespostas.ActivePageIndex := 3;
  end;
end;

procedure TfrmACBreSocial.btnConsIdeEveTabClick(Sender: TObject);
var
  TipoEvento, Chave, DataInicial, DataFinal: string;
  i: Integer;
  Ok: Boolean;
begin
  TipoEvento := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Tabela', 'Tipo do Evento no formato S-xxxx', TipoEvento)) then
    Exit;

  Chave := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Tabela', 'Chave do Evento', Chave)) then
    Exit;

  DataInicial := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Tabela', 'Data Inicial', DataInicial)) then
    Exit;

  DataFinal := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Tabela', 'Data Final', DataFinal)) then
    Exit;

  if ACBreSocial1.ConsultaIdentificadoresEventosTabela(edtIdEmpregador.Text,
                       StrToTipoEvento(Ok, TipoEvento), Chave,
                       StrToDateTimeDef(DataInicial, 0), StrToDateTimeDef(DataFinal, 0)) then
  begin

    MemoResp.Lines.Text := ACBreSocial1.WebServices.ConsultaIdentEventos.RetWS;

    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.ConsultaIdentEventos.PathNome);
        Add('');
        Add('Qtde Total de Eventos na Consulta: ' + IntToStr(RetIdentEvts.qtdeTotEvtsConsulta));
        Add('dhUltimo Evento Retornado: ' + DateTimeToStr(RetIdentEvts.dhUltimoEvtRetornado));

        for i := 0 to RetIdentEvts.Count - 1 do
        begin
          Add('Identificação do Evento:');
          Add('');
          Add(' - ID Evento: ' + RetIdentEvts.Items[i].Id);
          Add(' - Nr Recibo: ' + RetIdentEvts.Items[i].nrRec);
        end;
      end;
    end;
    pgRespostas.ActivePageIndex := 3;
  end;
end;

procedure TfrmACBreSocial.btnConsIdeEveTrabClick(Sender: TObject);
var
  CPFTrab, DataInicial, DataFinal: string;
  i: Integer;
begin
  CPFTrab := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Trabalhador', 'CPF Trabalhador', CPFTrab)) then
    Exit;

  DataInicial := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Trabalhador', 'Data Inicial', DataInicial)) then
    Exit;

  DataFinal := '';
  if not(InputQuery('WebServices: Consulta Identificadores Eventos Trabalhador', 'Data Final', DataFinal)) then
    Exit;

  if ACBreSocial1.ConsultaIdentificadoresEventosTrabalhador(edtIdEmpregador.Text, CPFTrab,
                       StrToDateTimeDef(DataInicial, 0), StrToDateTimeDef(DataFinal, 0)) then
  begin

    MemoResp.Lines.Text := ACBreSocial1.WebServices.ConsultaIdentEventos.RetWS;

    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.ConsultaIdentEventos.PathNome);
        Add('');
        Add('Qtde Total de Eventos na Consulta: ' + IntToStr(RetIdentEvts.qtdeTotEvtsConsulta));
        Add('dhUltimo Evento Retornado: ' + DateTimeToStr(RetIdentEvts.dhUltimoEvtRetornado));

        for i := 0 to RetIdentEvts.Count - 1 do
        begin
          Add('Identificação do Evento:');
          Add('');
          Add(' - ID Evento: ' + RetIdentEvts.Items[i].Id);
          Add(' - Nr Recibo: ' + RetIdentEvts.Items[i].nrRec);
        end;
      end;
    end;
    pgRespostas.ActivePageIndex := 3;
  end;
end;

procedure TfrmACBreSocial.btnDownloadEventosClick(Sender: TObject);
var
  PorID, PorNrRecibo: string;
  i: Integer;
begin
  PorID := '';
  if not(InputQuery('WebServices: Download Eventos', 'Por ID', PorID)) then
    Exit;

  PorNrRecibo := '';
  if not(InputQuery('WebServices: Download Eventos', 'Por Nr Recibo', PorNrRecibo)) then
    Exit;

  if PorID <> '' then
    PorNrRecibo := '';

  if PorNrRecibo <> '' then
    PorID := '';

  if ACBreSocial1.DownloadEventos(edtIdEmpregador.Text, PorID, PorNrRecibo) then
  begin

    MemoResp.Lines.Text := ACBreSocial1.WebServices.DownloadEventos.RetWS;

    with MemoDados.Lines do
    begin
      with ACBreSocial1.WebServices.DownloadEventos.RetDownloadEvt do
      begin
        Add('');
        Add('Código Retorno: ' + IntToStr(Status.cdResposta));
        Add('Mensagem: ' + Status.descResposta);
        Add('Arquivo Salvo: ' + ACBreSocial1.WebServices.DownloadEventos.PathNome);

        for i := 0 to arquivo.Count - 1 do
        begin
          Add('Arquivo:');
          Add('');
          Add('Código Retorno: ' + IntToStr(arquivo.Items[i].Status.cdResposta));
          Add('Mensagem: ' + arquivo.Items[i].Status.descResposta);
          Add('');
          Add(' - ID Evento: ' + arquivo.Items[i].Id);
          Add(' - Nr Recibo: ' + arquivo.Items[i].nrRec);
          Add('');
          Add('** Na propriedade: arquivo.Items[i].XML contem o XML do evento retornado');
          Add('** Esse XML também é salvo em disco.');
        end;
      end;
    end;
    pgRespostas.ActivePageIndex := 3;
  end;
end;

procedure TfrmACBreSocial.LimparDocsPasta;
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

procedure TfrmACBreSocial.SelecionaEventos;
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
  if (cbS2221.Checked) then
    GerareSocial2221;
  if (cbS2230.Checked) then
    GerareSocial2230;
  if (cbS2231.Checked) then
    GerareSocial2231;
  if (cbS2240.Checked) then
    GerareSocial2240;
  if (cbS2245.Checked) then
    GerareSocial2245;
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
  if (cbS2405.Checked) then
    GerareSocial2405;
  if (cbS2410.Checked) then
    GerareSocial2410;
  if (cbS2416.Checked) then
    GerareSocial2416;
  if (cbS2418.Checked) then
    GerareSocial2418;
  if (cbS2420.Checked) then
    GerareSocial2420;
  if (cbS2500.Checked) then
    GerareSocial2500;
  if (cbS2501.Checked) then
    GerareSocial2501;
  if (cbS3000.Checked) then
    GerareSocial3000;
end;

procedure TfrmACBreSocial.ACBreSocial1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
  Tratado := False;
end;

procedure TfrmACBreSocial.ACBreSocial1StatusChange(Sender: TObject);
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

end.
