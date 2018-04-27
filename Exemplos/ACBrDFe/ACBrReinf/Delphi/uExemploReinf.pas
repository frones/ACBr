{$I ACBr.inc}

unit uExemploReinf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons, IniFiles, Math, blcksock,
  StrUtils, TypInfo, FileCtrl,
  ACBrUtil, ACBrBase, ACBrDFe,
  ACBrReinf, ACBrReinfWebServices, ACBrReinfEventos, pcnConversaoReinf,
  pcnReinfR5001, pcnReinfR5011;

type
  TForm2 = class(TForm)
    Panel2: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    btnSalvarConfig: TBitBtn;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    PageControl4: TPageControl;
    TabSheet6: TTabSheet;
    lSSLLib: TLabel;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lXmlSign: TLabel;
    gbCertificado: TGroupBox;
    Label4: TLabel;
    Label6: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label25: TLabel;
    sbtnGetCert: TSpeedButton;
    SpeedButton1: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button10: TButton;
    GroupBox2: TGroupBox;
    Button6: TButton;
    cbAssinar: TCheckBox;
    Button7: TButton;
    Button9: TButton;
    cbSSLLib: TComboBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbXmlSignLib: TComboBox;
    TabSheet7: TTabSheet;
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
    TabSheet8: TTabSheet;
    GroupBox5: TGroupBox;
    Label7: TLabel;
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
    sbPathReinf: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathReinf: TEdit;
    edtPathEvento: TEdit;
    Panel3: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    chk1000: TCheckBox;
    chk2010: TCheckBox;
    chk2020: TCheckBox;
    chk2098: TCheckBox;
    chk1070: TCheckBox;
    chk2099: TCheckBox;
    chk9000: TCheckBox;
    chk2060: TCheckBox;
    chk2070: TCheckBox;
    TabSheet2: TTabSheet;
    mmoDados: TMemo;
    TabSheet3: TTabSheet;
    mmoXMLEnv: TMemo;
    TabSheet4: TTabSheet;
    mmoXMLRet: TMemo;
    Panel4: TPanel;
    btnGerar: TButton;
    GroupBox4: TGroupBox;
    edProtocolo: TEdit;
    Label1: TLabel;
    lblRecibo: TLabel;
    edRecibo: TEdit;
    lblEvento: TLabel;
    cbEvento: TComboBox;
    ChkRetificadora: TCheckBox;
    OpenDialog1: TOpenDialog;
    PageControl3: TPageControl;
    TabSheet9: TTabSheet;
    Label5: TLabel;
    edContNome: TEdit;
    Label26: TLabel;
    edContCPF: TEdit;
    Label27: TLabel;
    edContFone: TEdit;
    Label28: TLabel;
    edContCel: TEdit;
    Label39: TLabel;
    edContEmail: TEdit;
    TabSheet10: TTabSheet;
    Label40: TLabel;
    edSoftRazao: TEdit;
    Label41: TLabel;
    edSoftCNPJ: TEdit;
    Label43: TLabel;
    edSoftEmail: TEdit;
    Label44: TLabel;
    edSoftFone: TEdit;
    Label45: TLabel;
    edSoftContato: TEdit;
    edHash: TEdit;
    btnValidarSchema: TButton;
    btnValidarAssinatura: TButton;
    chk3010: TCheckBox;
    chk2030: TCheckBox;
    chk2040: TCheckBox;
    chk2050: TCheckBox;
    rdgOperacao: TRadioGroup;
    ACBrReinf1: TACBrReinf;
    btnConsultar: TButton;
    btnLerArqINI: TButton;
    btnEnviar: TButton;

    procedure btnGerarClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblColaboradorMouseEnter(Sender: TObject);
    procedure lblColaboradorMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure sbPathReinfClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure btnValidarSchemaClick(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure chk1000Click(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnLerArqINIClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure AtualizaSSLLibsCombo;

    procedure PreencherXMLEventos;
    procedure LimparDocsPasta;
    function GetTipoOperacao: TTipoOperacao;
    {Eventos}
    procedure GerarReinf1000;
    procedure GerarReinf1070;
    procedure GerarReinf2010;
    procedure GerarReinf2020;
    procedure GerarReinf2030;
    procedure GerarReinf2040;
    procedure GerarReinf2050;
    procedure GerarReinf2060;
    procedure GerarReinf2070;
    procedure GerarReinf2098;
    procedure GerarReinf2099;
    procedure GerarReinf3010;
    procedure GerarReinf9000;
  public
    procedure DepoisDeEnviar(const Axml: string);
    procedure AntesDeEnviar(const Axml: string);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  ACBrDFeSSL, pcnConversao, ShellAPI, Unit2;

const
  SELDIRHELP = 1000;
  
procedure TForm2.AntesDeEnviar(const Axml: string);
begin
  mmoXMLEnv.Clear;
  mmoXMLEnv.Lines.Text := Axml;
end;

procedure TForm2.btnConsultarClick(Sender: TObject);
var
  Protocolo: string;
  i, j: Integer;
  evtR5001: TR5001;
  evtR5011: TR5011;
begin
  Protocolo := '';
  if not(InputQuery('WebServices: Consulta Protocolo', 'Protocolo', Protocolo))
  then
    Exit;

  if ACBrReinf1.Consultar(Protocolo) then
  begin
    mmoXMLRet.Clear;
    mmoXMLRet.Lines.Text := ACBrReinf1.WebServices.Consultar.RetWS;

    with mmoDados.Lines do
    begin
      with ACBrReinf1.WebServices.Consultar.RetConsulta do
      begin
        Add('');

        for i := 0 to RetEventos.Count - 1 do
        begin
          Add(' Evento: ' + IntToStr(i));
          Add('   Tipo.........: ' + retEventos.Items[i].Tipo);
          case retEventos.Items[i].Evento.TipoEvento of
            teR5001:
              begin
                evtR5001 := TR5001(retEventos.Items[i].Evento.GetEvento);
                Add('   Id...........: ' + evtR5001.EvtTotal.Id);
                Add('   Cód Retorno..: ' + evtR5001.EvtTotal.IdeStatus.cdRetorno);
                Add('   Descrição....: ' + evtR5001.EvtTotal.IdeStatus.descRetorno);
              end;
            teR5011:
              begin
                evtR5011 := TR5011(retEventos.Items[i].Evento.GetEvento);
                with evtR5011.EvtTotalContrib do
                begin
                  Add('   Id...........: ' + Id);
                  Add('   Cód Retorno..: ' + IdeStatus.cdRetorno);
                  Add('   Descrição....: ' + IdeStatus.descRetorno);

                  Add(' **Ocorrencias');

                  for j := 0 to IdeStatus.regOcorrs.Count - 1 do
                  begin
                    with IdeStatus.regOcorrs.Items[j] do
                    begin
                      Add('   Tipo............: ' + Inttostr(tpOcorr));
                      Add('   Local Erro Aviso: ' + localErroAviso);
                      Add('   Código Resp.... : ' + codResp);
                      Add('   Descricao Resp..: ' + dscResp);
                    end;
                  end;
                end;
              end;
          end;
        end;
      end;
    end;

    PageControl1.ActivePageIndex := 1;
  end;
end;

procedure TForm2.btnEnviarClick(Sender: TObject);
var
  i: Integer;
  evtR5001: TR5001;
begin
//  edProtocolo.Text := '';
//  ACBrReinf1.Configuracoes.Geral.VersaoDF := TVersaoReinf(cbVersaoDF.ItemIndex);

//  ACBrReinf1.Eventos.Clear;
//  PreencherXMLEventos;
//  ACBrReinf1.AssinarEventos;

  if ACBrReinf1.Enviar then
  begin
    mmoXMLRet.Clear;
    mmoXMLRet.Lines.Text := ACBrReinf1.WebServices.EnvioLote.RetWS;

    with mmoDados.Lines do
    begin
      with ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
      begin
        Add('ideTransmissor: ' + IdeTransmissor.IdTransmissor);
        Add('cdStatus      : ' + IntToStr(Status.cdStatus));
        Add('descRetorno   : ' + Status.descRetorno);

        Add(' **Ocorrencias');

        for i := 0 to Status.Ocorrencias.Count - 1 do
        begin
          with Status.Ocorrencias.Items[i] do
          begin
            Add('   tipo: ' + Inttostr(tipo));
            Add('   localizacaoErroAviso: ' + localizacao);
            Add('   codigo: ' + inttostr(codigo));
            Add('   descricao: ' + descricao);
          end;
        end;

        Add('retornoEventos');

        for i:=0 to evento.Count - 1 do
        begin
          with evento.Items[i] do
          begin
            Add('Evento Id: ' + Id);

            evtR5001 := TR5001(Evento.GetEvento);
            Add('   Id...........: ' + evtR5001.EvtTotal.Id);
            Add('   Cód Retorno..: ' + evtR5001.EvtTotal.IdeStatus.cdRetorno);
            Add('   Descrição....: ' + evtR5001.EvtTotal.IdeStatus.descRetorno);
          end;
        end;
      end;
    end;

    PageControl1.ActivePageIndex := 1;
  end
  else
    ShowMessage('Falha');
end;

procedure TForm2.btnGerarClick(Sender: TObject);
var
  i: Integer;
  evtR5001: TR5001;
begin
  edProtocolo.Text := '';
  ACBrReinf1.Configuracoes.Geral.VersaoDF := TVersaoReinf(cbVersaoDF.ItemIndex);

  ACBrReinf1.Eventos.Clear;
  PreencherXMLEventos;
  ACBrReinf1.AssinarEventos;

  ShowMessage('XML dos Eventos Selecionados Gerados.');
end;

procedure TForm2.btnLerArqINIClick(Sender: TObject);
begin
  edProtocolo.Text := '';
  ACBrReinf1.Configuracoes.Geral.VersaoDF := TVersaoReinf(cbVersaoDF.ItemIndex);

  OpenDialog1.Title := 'Selecione o Evento (Arquivo INI)';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrReinf1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
    ACBrReinf1.Eventos.LoadFromINI(OpenDialog1.FileName);

  mmoDados.Lines.Add('INI de Eventos Carregado com Sucesso!');
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm2.DepoisDeEnviar(const Axml: string);
begin
  mmoXMLRet.Clear;
  mmoXMLRet.Lines.Text := Axml;
end;

procedure TForm2.LimparDocsPasta;
var
  path: string;
  FileOp: TSHFileOpStruct;
begin
  try
    path := edtPathReinf.Text;
    FillChar(FileOp, SizeOf(FileOp), 0);
    FileOp.wFunc := FO_DELETE;
    FileOp.pFrom := PChar(path+#0);//double zero-terminated
    FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
    SHFileOperation(FileOp);
    ForceDirectories(path);
  except
  end;
end;

procedure TForm2.PreencherXMLEventos;
begin
  if chk1000.Checked then
    GerarReinf1000;

  if chk1070.Checked then
    GerarReinf1070;

  if chk2010.Checked then
    GerarReinf2010;

  if chk2020.Checked then
    GerarReinf2020;

  if chk2030.Checked then
    GerarReinf2030;

  if chk2040.Checked then
    GerarReinf2040;

  if chk2050.Checked then
    GerarReinf2050;

  if chk2060.Checked then
    GerarReinf2060;

  if chk2070.Checked then
    GerarReinf2070;

  if chk2098.Checked then
    GerarReinf2098;

  if chk2099.Checked then
    GerarReinf2099;

  if chk3010.Checked then
    GerarReinf3010;

  if chk9000.Checked then
    GerarReinf9000;
end;


procedure TForm2.GerarReinf1000;
begin
  ACBrReinf1.Eventos.ReinfEventos.R1000.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R1000.Add do
  begin
    with evtInfoContri do
    begin
      Sequencial     := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb   := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      infoContribuinte.IdePeriodo.IniValid := '2017-01';
      infoContribuinte.IdePeriodo.FimValid := '2099-12';

      with infoContribuinte.InfoCadastro do
      begin
        ClassTrib          := ct11;
        indEscrituracao    := TindEscrituracao(0);
        indDesoneracao     := TindDesoneracao(1);
        indAcordoIsenMulta := TindAcordoIsenMulta(0);
        indSitPJ           := TindSitPJ(0);

        Contato.NmCtt    := edContNome.Text;
        Contato.CpfCtt   := edContCPF.Text;
        Contato.FoneFixo := edContFone.Text;
        Contato.FoneCel  := edContCel.Text;
        Contato.email    := edContEmail.Text;

        SoftwareHouse.Clear;
        with SoftwareHouse.Add do
        begin
          CnpjSoftHouse := '12345678000123';
          NmRazao       := 'SoftwareHouse Teste';
          NmCont        := 'Soft Contato';
          Telefone      := '1634335856';
          email         := 'teste@teste.com';
        end;
      end;

      infoContribuinte.NovaValidade.IniValid := '2017-01';
      infoContribuinte.NovaValidade.FimValid := '2099-12';
    end;
  end;
end;

procedure TForm2.GerarReinf1070;
begin
  ACBrReinf1.Eventos.ReinfEventos.R1070.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R1070.Add do
  begin
    with evtTabProcesso do
    begin
      Sequencial     := 0;
      ModoLancamento := GetTipoOperacao;

      IdeEvento.TpAmb   := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with infoProcesso do
      begin
        ideProcesso.tpProc     := tpAdministrativo;
        ideProcesso.nrProc     := '123';
        ideProcesso.IniValid   := '2017-01';
        ideProcesso.FimValid   := '2099-12';
        ideProcesso.indAutoria := taContribuinte;

        with ideProcesso do
        begin
          infoSusp.Clear;
          with infoSusp.Add do
          begin
            codSusp     := '12345678';
            indSusp     := siLiminarMandadoSeguranca;
            dtDecisao   := Date;
            indDeposito := tpSim;
          end;

          DadosProcJud.UfVara   := 'SP';
          DadosProcJud.codMunic := 1234567;
          DadosProcJud.idVara   := '12';
        end;

        NovaValidade.IniValid := '2017-01';
        NovaValidade.FimValid := '2099-12';
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2010;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2010.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2010.Add do
  begin
    with evtServTom do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with infoServTom do
      begin
        with ideEstabObra do
        begin
          tpInscEstab := tiCNPJ;
          nrInscEstab := '123';
          indObra     := ioNaoeObraDeConstrucaoCivil;

          with idePrestServ do
          begin
            cnpjPrestador     := '12345678000123';
            vlrTotalBruto     := 10.00;
            vlrTotalBaseRet   := 10.00;
            vlrTotalRetPrinc  := 10.00;
            vlrTotalRetAdic   := 10.00;
            vlrTotalNRetPrinc := 10.00;
            vlrTotalNRetAdic  := 10.00;
            indCPRB           := icNaoContribuintePrevidenciariaReceitaBruta;

            nfs.Clear;
            with nfs.Add do
            begin
              serie       := '1';
              numDocto    := '123';
              dtEmissaoNF := Date;
              vlrBruto    := 1000.00;
              obs         := '';

              infoTpServ.Clear;
              with infoTpServ.Add do
              begin
                tpServico     := '100000003'; {Tabela 06}
                vlrBaseRet    := 100.00;
                vlrRetencao   := 11.00;
                vlrRetSub     := 0.00;
                vlrNRetPrinc  := 0.00;
                vlrServicos15 := 0.00;
                vlrServicos20 := 0.00;
                vlrServicos25 := 0.00;
                vlrAdicional  := 0.00;
                vlrNRetAdic   := 0.00;
              end;
            end;

            infoProcRetPr.Clear;
            with infoProcRetPr.Add do
            begin
              tpProcRetPrinc := tpAdministrativo;
              nrProcRetPrinc := '1122112';
              codSuspPrinc   := 001;
              valorPrinc     := 100.00;
            end;

            infoProcRetAd.Clear;
            with infoProcRetAd.Add do
            begin
              tpProcRetAdic := tpAdministrativo;
              nrProcRetAdic := '1122112';
              codSuspAdic   := 001;
              valorAdic     := 1000.00;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2020;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2020.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2020.Add do
  begin
    with evtServPrest do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with infoServPrest do
      begin
        with ideEstabPrest do
        begin
          tpInscEstabPrest := tiCNPJ;
          nrInscEstabPrest := '12345678000123';

          with ideTomador do
          begin
            tpInscTomador     := tiCNPJ;
            nrInscTomador     := '12345678000123';
            indObra           := ioNaoeObraDeConstrucaoCivil;
            vlrTotalBruto     := 10.00;
            vlrTotalBaseRet   := 10.00;
            vlrTotalRetPrinc  := 10.00;
            vlrTotalRetAdic   := 10.00;
            vlrTotalNRetPrinc := 10.00;
            vlrTotalNRetAdic  := 10.00;

            nfs.Clear;
            with nfs.Add do
            begin
              serie       := '1';
              numDocto    := '123';
              dtEmissaoNF := Date;
              vlrBruto    := 1000.00;
              obs         := '';

              infoTpServ.Clear;
              with infoTpServ.Add do
              begin
                tpServico     := '100000003'; {Tabela 06}
                vlrBaseRet    := 100.00;
                vlrRetencao   := 11.00;
                vlrRetSub     := 0.00;
                vlrNRetPrinc  := 0.00;
                vlrServicos15 := 0.00;
                vlrServicos20 := 0.00;
                vlrServicos25 := 0.00;
                vlrAdicional  := 0.00;
                vlrNRetAdic   := 0.00;
              end;
            end;

            infoProcRetPr.Clear;
            with infoProcRetPr.Add do
            begin
              tpProcRetPrinc := tpAdministrativo;
              nrProcRetPrinc := '1122112';
              codSuspPrinc   := 001;
              valorPrinc     := 100.00;
            end;

            infoProcRetAd.Clear;
            with infoProcRetAd.Add do
            begin
              tpProcRetAdic := tpAdministrativo;
              nrProcRetAdic := '1122112';
              codSuspAdic   := 001;
              valorAdic     := 1000.00;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2030;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2030.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2030.Add do
  begin
    with evtAssocDespRec do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with ideEstab do
      begin
        tpInscEstab := tiCNPJ;
        nrInscEstab := '12345678000123';

        recursosRec.Clear;
        with recursosRec.Add do
        begin
          cnpjOrigRecurso := '12345678000123';
          vlrTotalRec     := 100.00;
          vlrTotalRet     := 0;
          vlrTotalNRet    := 0;

          infoRecurso.Clear;
          with infoRecurso.Add do
          begin
            tpRepasse   := trPatrocinio;
            descRecurso := 'descricao resumida';
            vlrBruto    := 11.00;
            vlrRetApur  := 0.00;
          end;

          infoProc.Clear;
          with infoProc.Add do
          begin
            tpProc  := tpAdministrativo;
            nrProc  := '123';
            codSusp := '456';
            vlrNRet := 0.00;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2040;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2040.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2040.Add do
  begin
    with evtAssocDespRep do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with ideEstab do
      begin
        tpInscEstab := tiCNPJ;
        nrInscEstab := '12345678000123';

        recursosRep.Clear;
        with recursosRep.Add do
        begin
          cnpjAssocDesp := '12345678000123';
          vlrTotalRep   := 100.00;
          vlrTotalRet   := 0;
          vlrTotalNRet  := 0;

          infoRecurso.Clear;
          with infoRecurso.Add do
          begin
            tpRepasse   := trPatrocinio;
            descRecurso := 'descricao resumida';
            vlrBruto    := 11.00;
            vlrRetApur  := 0.00;
          end;

          infoProc.Clear;
          with infoProc.Add do
          begin
            tpProc  := tpAdministrativo;
            nrProc  := '123';
            codSusp := '456';
            vlrNRet := 0.00;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2050;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2050.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2050.Add do
  begin
    with evtComProd do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with infoComProd.ideEstab do
      begin
        tpInscEstab       := tiCNPJ;
        nrInscEstab       := '12345678000123';
        vlrRecBrutaTotal  := 100.00;
        vlrCPApur         := 100.00;
        vlrRatApur        := 100.00;
        vlrSenarApur      := 100.00;
        vlrCPSuspTotal    := 100.00;
        vlrRatSuspTotal   := 100.00;
        vlrSenarSuspTotal := 100.00;

        tipoCom.Clear;
        with tipoCom.Add do
        begin
          indCom      := icProdRural;
          vlrRecBruta := 100.50;

          infoProc.Clear;
          with infoProc.Add do
          begin
            tpProc       := tpAdministrativo;
            nrProc       := '123';
            codSusp      := '456';
            vlrCPSusp    := 0.00;
            vlrRatSusp   := 0.00;
            vlrSenarSusp := 0.00;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2060;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2060.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2060.Add do
  begin
    with evtCPRB do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with infoCPRB.ideEstab do
      begin
        tpInscEstab      := tiCNPJ;
        nrInscEstab      := '12345678000123';
        vlrRecBrutaTotal := 100.00;
        vlrCPApurTotal   := 100.00;
        vlrCPRBSuspTotal := 100.00;

        tipoCod.Clear;
        with tipoCod.Add do
        begin
          codAtivEcon     := '12345678';
          vlrRecBrutaAtiv := 100.50;
          vlrExcRecBruta  := 100.50;
          vlrAdicRecBruta := 100.50;
          vlrBcCPRB       := 100.50;
          vlrCPRBapur     := 100.50;

          tipoAjuste.Clear;
          with tipoAjuste.Add do
          begin
            tpAjuste   := taReducao;
            codAjuste  := caRegimeCaixa;
            vlrAjuste  := 0.00;
            descAjuste := 'descricao';
            dtAjuste   := '2018-04';
          end;

          infoProc.Clear;
          with infoProc.Add do
          begin
            tpProc      := tpAdministrativo;
            nrProc      := '123';
            codSusp     := '456';
            vlrCPRBSusp := 0.00;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2070;
begin
  // EVENTO NÃO DISPONIBILIZADO ATÉ A VERSÃO 1_02

//  EXIT;

  ACBrReinf1.Eventos.ReinfEventos.R2070.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2070.Add do
  begin
    with evtPgtosDivs do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;
      ideEvento.NrRecibo := '123';
      ideEvento.perApur  := '2018-04';
      IdeEvento.TpAmb    := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi  := peAplicEmpregador;
      IdeEvento.VerProc  := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with ideBenef do
      begin
        codPgto      := '123';
        tpInscBenef  := tiCNPJ;
        nrInscBenef  := '12345678000123';
        nmRazaoBenef := 'Nome do Beneficiario';

        with infoResidExt do
        begin
          with infoEnder do
          begin
            paisResid := 'Estados Unidos da America';
            dscLograd := 'avenida';
            nrLograd  := '100';
            complem   := '';
            bairro    := 'centro';
            cidade    := 'New York';
            codPostal := '123';
          end;

          with infoFiscal do
          begin
            indNIF        := nifCom;
            nifBenef      := '1234';
            relFontePagad := '1245';
          end;
        end;

        with infoMolestia do
        begin
          dtLaudo := Date;
        end;

        ideEstab.Clear;
        with ideEstab.Add do
        begin
          tpInsc := tiCNPJ;
          nrInsc := '12345678000112';

          pgtoPF.Clear;
          with pgtoPF.Add do
          begin
            dtPgto            := Date;
            indSuspExig       := tpSim;
            indDecTerceiro    := tpSim;
            vlrRendTributavel := 0.0;

            detDeducao.Clear;
            with detDeducao.Add do
            begin
              indTpDeducao := itdOficial;
              vlrDeducao   := 0.0;
            end;

            rendIsento.Clear;
            with rendIsento.Add do
            begin
              tpIsencao      := tiIsenta;
              vlrIsento      := 0.0;
              descRendimento := '';
            end;

            detCompet.Clear;
            with detCompet.Add do
            begin
              indPerReferencia  := iprMensal;
              perRefPagto       := '2018-04';
              vlrRendTributavel := 0.0;
            end;

            with compJud do
            begin
              vlrCompAnoCalend := 0.0;
              vlrCompAnoAnt    := 0.0;
            end;

            infoRRA.Clear;
            with infoRRA.Add do
            begin
              tpProcRRA   := tpAdministrativo;
              nrProcRRA   := '1234';
              codSusp     := '12345';
              natRRA      := 'natureza';
              qtdMesesRRA := 1;

              with despProcJud do
              begin
                vlrDespCustas    := 0.0;
                vlrDespAdvogados := 0.0;

                ideAdvogado.Clear;
                with ideAdvogado.Add do
                begin
                  tpInscAdvogado := tiCNPJ;
                  nrInscAdvogado := '12345678000123';
                  vlrAdvogado    := 0.0;
                end;
              end;
            end;

            infoProcJud.Clear;
            with infoProcJud.Add do
            begin
              nrProcJud         := '1234';
              codSusp           := '123';
              indOrigemRecursos := iorProprios;

              with despProcJud do
              begin
                vlrDespCustas    := 0.0;
                vlrDespAdvogados := 0.0;

                ideAdvogado.Clear;
                with ideAdvogado.Add do
                begin
                  tpInscAdvogado := tiCNPJ;
                  nrInscAdvogado := '12345678000123';
                  vlrAdvogado    := 0.0;
                end;
              end;

              with origemRecursos do
              begin
                cnpjOrigemRecursos := '12345678000123';
              end;
            end;

            with depJudicial do
            begin
              vlrDepJudicial := 0.0
            end;
          end;

          pgtoPJ.Clear;
          with pgtoPJ.Add do
          begin
            dtPagto           := Date;
            vlrRendTributavel := 0.0;
            vlrRet            := 0.0;

            infoProcJud.Clear;
            with infoProcJud.Add do
            begin
              nrProcJud         := '1234';
              codSusp           := '123';
              indOrigemRecursos := iorProprios;

              with despProcJud do
              begin
                vlrDespCustas    := 0.0;
                vlrDespAdvogados := 0.0;

                ideAdvogado.Clear;
                with ideAdvogado.Add do
                begin
                  tpInscAdvogado := tiCNPJ;
                  nrInscAdvogado := '12345678000123';
                  vlrAdvogado    := 0.0;
                end;
              end;

              with origemRecursos do
              begin
                cnpjOrigemRecursos := '12345678000123';
              end;
            end;
          end;

          with pgtoResidExt do
          begin
            dtPagto         := Date;
            tpRendimento    := '123';
            formaTributacao := '123';
            vlrPgto         := 0.0;
            vlrRet          := 0.0;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf2098;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2098.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2098.Add do
  begin
    with evtReabreEvPer do
    begin
      Sequencial := 0;

      ideEvento.perApur := '2018-04';
      IdeEvento.TpAmb   := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;
    end;
  end;
end;

procedure TForm2.GerarReinf2099;
begin
  ACBrReinf1.Eventos.ReinfEventos.R2099.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R2099.Add do
  begin
    with evtFechaEvPer do
    begin
      Sequencial := 0;

      ideEvento.perApur := '2018-04';
      IdeEvento.TpAmb   := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with ideRespInf do
      begin
        nmResp   := edContNome.Text;
        cpfResp  := edContCPF.Text;
        telefone := edContFone.Text;
        email    := edContEmail.Text;
      end;

      with infoFech do
      begin
        evtServTm     := tpSim;
        evtServPr     := tpSim;
        evtAssDespRec := tpSim;
        evtAssDespRep := tpSim;
        evtComProd    := tpSim;
        evtCPRB       := tpSim;
        evtPgtos      := tpNao;

        compSemMovto := '2017-01'; {Somente preenchido se os outros valores forem tbNao}
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf3010;
begin
  ACBrReinf1.Eventos.ReinfEventos.R3010.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R3010.Add do
  begin
    with evtEspDesportivo do
    begin
      Sequencial := 0;

      ideEvento.indRetif := trOriginal;

      if ChkRetificadora.Checked then
        ideEvento.indRetif := trRetificacao;

      if ideEvento.indRetif = trRetificacao then
        ideEvento.nrRecibo := edRecibo.Text;

      ideEvento.dtApuracao := Date;
      IdeEvento.TpAmb      := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi    := peAplicEmpregador;
      IdeEvento.VerProc    := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      ideEstab.Clear;
      with ideEstab.Add do
      begin
        tpInscEstab := tiCNPJ;
        nrInscEstab := edtEmitCNPJ.Text;

        boletim.Clear;
        with boletim.Add do
        begin
          nrBoletim       := '1234';
          tpCompeticao    := ttcOficial;
          categEvento     := tceLocal;
          modDesportiva   := 'TESTE';
          nomeCompeticao  := 'TESTE';
          cnpjMandante    := edtEmitCNPJ.Text;
          cnpjVisitante   := '99999999999999';
          nomeVisitante   := 'TESTE';
          pracaDesportiva := 'TESTE';
          codMunic        := 3550308;
          uf              := 'SP';
          qtdePagantes    := 999;
          qtdeNaoPagantes := 999;

          receitaIngressos.Clear;
          with receitaIngressos.Add do
          begin
            tpIngresso       := ttiArquibancada;
            descIngr         := 'TESTE';
            qtdeIngrVenda    := 999;
            qtdeIngrVendidos := 999;
            qtdeIngrDev      := 0;
            precoIndiv       := 1;
            vlrTotal         := 999;
          end;

          outrasReceitas.Clear;
          with outrasReceitas.Add do
          begin
            tpReceita   := ttrTransmissao;
            vlrReceita  := 1234;
            descReceita := 'TESTE'
          end;
        end;

        with receitaTotal do
        begin
          vlrReceitaTotal  := 999;
          vlrCP            := 0;
          vlrCPSuspTotal   := 0;
          vlrReceitaClubes := 0;
          vlrRetParc       := 0;

          infoProc.Clear;
          with infoProc.Add do
          begin
            tpProc    := tpAdministrativo;
            nrProc    := '1234567890';
            codSusp   := '1234';
            vlrCPSusp := 1234;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm2.GerarReinf9000;
begin
  ACBrReinf1.Eventos.ReinfEventos.R9000.Clear;
  with ACBrReinf1.Eventos.ReinfEventos.R9000.Add do
  begin
    with evtExclusao do
    begin
      Sequencial := 0;

      IdeEvento.TpAmb   := taProducaoRestritaDadosReais;
      IdeEvento.ProcEmi := peAplicEmpregador;
      IdeEvento.VerProc := '1.0';

      ideContri.TpInsc := tiCNPJ;
      ideContri.NrInsc := edtEmitCNPJ.Text;

      with infoExclusao do
      begin
        tpEvento := cbEvento.Items.Strings[cbEvento.ItemIndex];
        nrRecEvt := Trim(edRecibo.Text);

        if ( cbEvento.Text = 'R-3010' ) then
          perApur := FormatDateTime( 'yyyy-mm-dd', Now )
        else
          perApur := FormatDateTime( 'yyyy-mm', Now );
      end;
    end;
  end;
end;

function TForm2.GetTipoOperacao: TTipoOperacao;
begin
  case rdgOperacao.ItemIndex of
    1: Result := toAlteracao;
    2: Result := toExclusao;
  else
    Result := toInclusao;
  end;
end;

procedure TForm2.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TForm2.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/35');
end;

procedure TForm2.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TForm2.lblColaboradorMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TForm2.lblColaboradorMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TForm2.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
      Ini.WriteInteger( 'Certificado','SSLLib' , cbSSLLib.ItemIndex);
      Ini.WriteInteger( 'Certificado','CryptLib' , cbCryptLib.ItemIndex);
      Ini.WriteInteger( 'Certificado','HttpLib' , cbHttpLib.ItemIndex);
      Ini.WriteInteger( 'Certificado','XmlSignLib' , cbXmlSignLib.ItemIndex);
      Ini.WriteString( 'Certificado','Caminho' ,edtCaminho.Text);
      Ini.WriteString( 'Certificado','Senha'   ,edtSenha.Text);
      Ini.WriteString( 'Certificado','NumSerie',edtNumSerie.Text);

      Ini.WriteBool(   'Geral','AtualizarXML'      ,cbxAtualizarXML.Checked);
      Ini.WriteBool(   'Geral','ExibirErroSchema'  ,cbxExibirErroSchema.Checked);
      Ini.WriteString( 'Geral','FormatoAlerta'  ,edtFormatoAlerta.Text);
      Ini.WriteInteger( 'Geral','FormaEmissao',cbFormaEmissao.ItemIndex);
      Ini.WriteInteger( 'Geral','VersaoDF',cbVersaoDF.ItemIndex);
      Ini.WriteBool(   'Geral','RetirarAcentos'      ,cbxRetirarAcentos.Checked);
      Ini.WriteBool(   'Geral','Salvar'      ,ckSalvar.Checked);
      Ini.WriteString( 'Geral','PathSalvar'  ,edtPathLogs.Text);
      Ini.WriteString( 'Geral','PathSchemas'  ,edtPathSchemas.Text);

      Ini.WriteString( 'WebService','UF'        ,cbUF.Text);
      Ini.WriteInteger( 'WebService','Ambiente'  ,rgTipoAmb.ItemIndex);
      Ini.WriteBool(   'WebService','Visualizar',cbxVisualizar.Checked);
      Ini.WriteBool(   'WebService','SalvarSOAP',cbxSalvarSOAP.Checked);
      Ini.WriteBool(   'WebService','AjustarAut',cbxAjustarAut.Checked);
      Ini.WriteString( 'WebService','Aguardar'    ,edtAguardar.Text);
      Ini.WriteString( 'WebService','Tentativas'  ,edtTentativas.Text);
      Ini.WriteString( 'WebService','Intervalo'  ,edtIntervalo.Text);
      Ini.WriteInteger( 'WebService','TimeOut'   ,seTimeOut.Value);
      Ini.WriteInteger( 'WebService','SSLType' , cbSSLType.ItemIndex);

      Ini.WriteString( 'Proxy','Host'   ,edtProxyHost.Text);
      Ini.WriteString( 'Proxy','Porta'  ,edtProxyPorta.Text);
      Ini.WriteString( 'Proxy','User'   ,edtProxyUser.Text);
      Ini.WriteString( 'Proxy','Pass'   ,edtProxySenha.Text);

      Ini.WriteBool(   'Arquivos','Salvar'          ,cbxSalvarArqs.Checked);
      Ini.WriteBool(   'Arquivos','PastaMensal'     ,cbxPastaMensal.Checked);
      Ini.WriteBool(   'Arquivos','AddLiteral'      ,cbxAdicionaLiteral.Checked);
      Ini.WriteBool(   'Arquivos','SalvarPathEvento',cbxSalvaPathEvento.Checked);
      Ini.WriteBool(   'Arquivos','SepararPorCNPJ'  ,cbxSepararPorCNPJ.Checked);
      Ini.WriteString( 'Arquivos','PathReinf'  ,edtPathReinf.Text);
      Ini.WriteString( 'Arquivos','PathEvento' ,edtPathEvento.Text);

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

      Ini.WriteString( 'Contato', 'Nome'        ,edContNome.Text);
      Ini.WriteString( 'Contato', 'CPF'         ,edContCPF.Text);
      Ini.WriteString( 'Contato', 'Fone'        ,edContFone.Text);
      Ini.WriteString( 'Contato', 'Celular'     ,edContCel.Text);
      Ini.WriteString( 'Contato', 'Email'       ,edContEmail.Text);

      Ini.WriteString( 'SofHouse', 'RazaoSocial',edSoftRazao.Text);
      Ini.WriteString( 'SofHouse', 'CNPJ'       ,edSoftCNPJ.Text);
      Ini.WriteString( 'SofHouse', 'Email'      ,edSoftEmail.Text);
      Ini.WriteString( 'SofHouse', 'Fone'       ,edSoftFone.Text);
      Ini.WriteString( 'SofHouse', 'Contato'    ,edSoftContato.Text);
  finally
     Ini.Free;
  end;
end;

procedure TForm2.LerConfiguracao;
var
  IniFile, PathMensal: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
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
    edtEmitCidade.Text     :=Ini.ReadString( 'Emitente','Cidade'     ,'');
    edtEmitUF.Text         := Ini.ReadString( 'Emitente','UF'         ,'');

    edContNome.Text        := Ini.ReadString( 'Contato', 'Nome'        ,'');
    edContCPF.Text         := Ini.ReadString( 'Contato', 'CPF'         ,'');
    edContFone.Text        := Ini.ReadString( 'Contato', 'Fone'        ,'');
    edContCel.Text         := Ini.ReadString( 'Contato', 'Celular'     ,'');
    edContEmail.Text       := Ini.ReadString( 'Contato', 'Email'       ,'');

    edSoftRazao.Text       := Ini.ReadString( 'SofHouse', 'RazaoSocial','');
    edSoftCNPJ.Text        := Ini.ReadString( 'SofHouse', 'CNPJ'       ,'');
    edSoftEmail.Text       := Ini.ReadString( 'SofHouse', 'Email'      ,'');
    edSoftFone.Text        := Ini.ReadString( 'SofHouse', 'Fone'       ,'');
    edSoftContato.Text     := Ini.ReadString( 'SofHouse', 'Contato'    ,'');

    cbSSLLib.ItemIndex:= Ini.ReadInteger( 'Certificado','SSLLib' ,0);
    cbCryptLib.ItemIndex := Ini.ReadInteger( 'Certificado','CryptLib' , 0);
    cbHttpLib.ItemIndex := Ini.ReadInteger( 'Certificado','HttpLib' , 0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger( 'Certificado','XmlSignLib' , 0);
    edtCaminho.Text  := Ini.ReadString( 'Certificado','Caminho' ,'');
    edtSenha.Text    := Ini.ReadString( 'Certificado','Senha'   ,'');
    edtNumSerie.Text := Ini.ReadString( 'Certificado','NumSerie','');

    ACBrReinf1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
    ACBrReinf1.Configuracoes.Certificados.Senha       := edtSenha.Text;
    ACBrReinf1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;
    ACBrReinf1.Configuracoes.Certificados.VerificarValidade := True;

    cbxAtualizarXML.Checked    := Ini.ReadBool(   'Geral','AtualizarXML',True);
    cbxExibirErroSchema.Checked    := Ini.ReadBool(   'Geral','ExibirErroSchema',True);
    edtFormatoAlerta.Text    := Ini.ReadString( 'Geral','FormatoAlerta'  ,'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex := Ini.ReadInteger( 'Geral','FormaEmissao',0);
    cbVersaoDF.ItemIndex := Ini.ReadInteger( 'Geral','VersaoDF',0);
    ckSalvar.Checked     := Ini.ReadBool(   'Geral','Salvar',True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral','RetirarAcentos',True);
    edtPathLogs.Text     := Ini.ReadString( 'Geral','PathSalvar'  ,PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text  := Ini.ReadString( 'Geral','PathSchemas'  ,PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\Reinf');

    with ACBrReinf1.Configuracoes.Geral do
    begin
      VersaoDF := TVersaoReinf(cbVersaoDF.ItemIndex);
      SSLLib                := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib           := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib            := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib         := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      AtualizaSSLLibsCombo;
      ExibirErroSchema := cbxExibirErroSchema.Checked;
      RetirarAcentos   := cbxRetirarAcentos.Checked;
      FormatoAlerta    := edtFormatoAlerta.Text;
      FormaEmissao     := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);
      Salvar           := ckSalvar.Checked;
      IdContribuinte := edtEmitCNPJ.Text;
    end;

    cbUF.ItemIndex        := cbUF.Items.IndexOf(Ini.ReadString( 'WebService','UF','SP'));
    rgTipoAmb.ItemIndex   := Ini.ReadInteger( 'WebService','Ambiente'  ,0);
    cbxVisualizar.Checked  := Ini.ReadBool(    'WebService','Visualizar',False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(    'WebService','SalvarSOAP',False);
    cbxAjustarAut.Checked  := Ini.ReadBool(   'WebService','AjustarAut' ,False);
    edtAguardar.Text       := Ini.ReadString( 'WebService','Aguardar'  ,'0');
    edtTentativas.Text     := Ini.ReadString( 'WebService','Tentativas','5');
    edtIntervalo.Text      := Ini.ReadString( 'WebService','Intervalo' ,'0');
    seTimeOut.Value        := Ini.ReadInteger('WebService','TimeOut'  ,5000);
    cbSSLType.ItemIndex    := Ini.ReadInteger('WebService','SSLType' , 0);
    edtProxyHost.Text  := Ini.ReadString( 'Proxy','Host'   ,'');
    edtProxyPorta.Text := Ini.ReadString( 'Proxy','Porta'  ,'');
    edtProxyUser.Text  := Ini.ReadString( 'Proxy','User'   ,'');
    edtProxySenha.Text := Ini.ReadString( 'Proxy','Pass'   ,'');

    with ACBrReinf1.SSL do
    begin
      DescarregarCertificado;
      SSLDgst := dgstSHA256;
      SSLType := TSSLType( cbSSLType.ItemIndex );
    end;

    with ACBrReinf1.Configuracoes.WebServices do
    begin
      if ( rgTipoAmb.ItemIndex = 0 ) then
        Ambiente := taProducao
      else
        Ambiente := taHomologacao;

      UF         := cbUF.Text;
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
        edtIntervalo.Text := IntToStr(ACBrReinf1.Configuracoes.WebServices.IntervaloTentativas);

      TimeOut := seTimeOut.Value;

      ProxyHost := edtProxyHost.Text;
      ProxyPort := edtProxyPorta.Text;
      ProxyUser := edtProxyUser.Text;
      ProxyPass := edtProxySenha.Text;
    end;

    cbxSalvarArqs.Checked       := Ini.ReadBool(   'Arquivos','Salvar'     ,false);
    cbxPastaMensal.Checked      := Ini.ReadBool(   'Arquivos','PastaMensal',false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(   'Arquivos','AddLiteral' ,false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(   'Arquivos','SalvarPathEvento',false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(   'Arquivos','SepararPorCNPJ',false);
    edtPathReinf.Text           := Ini.ReadString( 'Arquivos','PathReinf' ,'');
    edtPathEvento.Text          := Ini.ReadString( 'Arquivos','PathEvento','');

    with ACBrReinf1.Configuracoes.Arquivos do
    begin
      SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
      Salvar           := cbxSalvarArqs.Checked;
      SepararPorMes    := cbxPastaMensal.Checked;
      AdicionarLiteral := cbxAdicionaLiteral.Checked;
      PathSalvar       := edtPathLogs.Text;
      PathSchemas      := edtPathSchemas.Text;
    end;

    PathMensal := ACBrReinf1.Configuracoes.Arquivos.GetPathReinf(0);

    ACBrReinf1.Configuracoes.Arquivos.PathSalvar := PathMensal;
  finally
     Ini.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
 T: TSSLLib;
 I: TpcnTipoEmissao;
 U: TSSLCryptLib;
 V: TSSLHttpLib;
 X: TSSLXmlSignLib;
 Y: TSSLType;
 R: TVersaoReinf;
begin
  mmoDados.Clear;
  mmoXMLRet.Clear;
  mmoXMLEnv.Clear;

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
  for R := Low(TVersaoReinf) to High(TVersaoReinf) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoReinf), integer(R) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  PageControl1.ActivePageIndex := 0;

  ACBrReinf1.Configuracoes.WebServices.Salvar := true;
end;

procedure TForm2.AtualizaSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer( ACBrReinf1.Configuracoes.Geral.SSLLib );
  cbCryptLib.ItemIndex   := Integer( ACBrReinf1.Configuracoes.Geral.SSLCryptLib );
  cbHttpLib.ItemIndex    := Integer( ACBrReinf1.Configuracoes.Geral.SSLHttpLib );
  cbXmlSignLib.ItemIndex := Integer( ACBrReinf1.Configuracoes.Geral.SSLXmlSignLib );

  cbSSLType.Enabled := (ACBrReinf1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TForm2.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  LerConfiguracao;
end;

procedure TForm2.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title      := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter     := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    ACBrReinf1.SSL.LerCertificadosStore;
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

    For I := 0 to ACBrReinf1.SSL.ListaCertificados.Count-1 do
    begin
      with ACBrReinf1.SSL.ListaCertificados[I] do
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

procedure TForm2.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrReinf1.SSL.SelecionarCertificado;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  ShowMessage( FormatDateBr(ACBrReinf1.SSL.CertDataVenc) );
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  ShowMessage( ACBrReinf1.SSL.CertNumeroSerie );
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  ShowMessage( ACBrReinf1.SSL.CertSubjectName + sLineBreak + sLineBreak +
               'Razão Social: '+ACBrReinf1.SSL.CertRazaoSocial);   
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  ShowMessage( ACBrReinf1.SSL.CertCNPJ );
end;

procedure TForm2.Button10Click(Sender: TObject);
begin
 ShowMessage( ACBrReinf1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: '+ACBrReinf1.SSL.CertCertificadora);
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrReinf1.SSL.CalcHash(edHash.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  mmoDados.Lines.Add( Ahash );
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm2.Button7Click(Sender: TObject);
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

  OldUseCert := ACBrReinf1.SSL.UseCertificateHTTP;
  ACBrReinf1.SSL.UseCertificateHTTP := False;
  try
    mmoDados.Lines.Text := ACBrReinf1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrReinf1.SSL.UseCertificateHTTP := OldUseCert;
  end;
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm2.Button9Click(Sender: TObject);
begin
  with ACBrReinf1.SSL do
  begin
    with mmoDados do
    begin
      CarregarCertificadoPublico(Lines.Text);
      Lines.Add(CertIssuerName);
      Lines.Add(CertRazaoSocial);
      Lines.Add(CertCNPJ);
      Lines.Add(CertSubjectName);
      Lines.Add(CertNumeroSerie);
     end;
    PageControl1.ActivePageIndex := 1;
  end;
end;

procedure TForm2.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TForm2.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TForm2.PathClick(Sender: TObject);
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

procedure TForm2.sbPathReinfClick(Sender: TObject);
begin
  PathClick(edtPathReinf);
end;

procedure TForm2.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TForm2.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrReinf1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TForm2.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrReinf1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TForm2.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrReinf1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TForm2.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrReinf1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TForm2.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    ACBrReinf1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TForm2.btnValidarSchemaClick(Sender: TObject);
var
  tsAux1: TStringList;
  Erro: String;
begin
  OpenDialog1.Title := 'Selecione o Arquivo';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if not OpenDialog1.Execute then
    exit;

  tsAux1 := TStringList.Create;
  tsAux1.LoadFromFile( OpenDialog1.FileName );

  OpenDialog1.Title := 'Selecione o Schema';
  OpenDialog1.DefaultExt := '*.XSD';
  OpenDialog1.Filter := 'Arquivos XSD (*.XSD)|*.XSD|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if not OpenDialog1.Execute then
    exit;

  ACBrReinf1.SSL.Validar(tsAux1.Text, // Copy( tsAux1.Text, 1, Length(tsAux1.Text) - 2 ),
                         OpenDialog1.FileName,
                         Erro);
  FreeAndNil( tsAux1 );
  ShowMessage(Erro);
end;

procedure TForm2.btnValidarAssinaturaClick(Sender: TObject);
var
  tsAux1: TStringList;
  Erro: String;
begin
  OpenDialog1.Title := 'Selecione o Arquivo';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if not OpenDialog1.Execute then
    exit;

  tsAux1 := TStringList.Create;
  tsAux1.LoadFromFile( OpenDialog1.FileName );

  if ACBrReinf1.SSL.VerificarAssinatura(Copy( tsAux1.Text, 1, Length(tsAux1.Text) - 2 ),
                                     Erro,
                                     '',
                                     'Signature') then
    ShowMessage('OK')
  else
    ShowMessage(ifthen( Erro = '', 'Assinatura Inválida', Erro ));

  FreeAndNil( tsAux1 );
end;

procedure TForm2.chk1000Click(Sender: TObject);
begin
  rdgOperacao.Visible     := ( chk1000.Checked or
                               chk1070.Checked );

  ChkRetificadora.Visible := ( chk2010.Checked or
                               chk2020.Checked or
                               chk2030.Checked or
                               chk2040.Checked or
                               chk2050.Checked or
                               chk2060.Checked or
                               chk2070.Checked or
                               chk3010.Checked );

  edRecibo.Visible        := ( chk2010.Checked or
                               chk2020.Checked or
                               chk2030.Checked or
                               chk2040.Checked or
                               chk2050.Checked or
                               chk2060.Checked or
                               chk2070.Checked or
                               chk3010.Checked or
                               chk9000.Checked );

  lblRecibo.Visible       := edRecibo.Visible;

  cbEvento.Visible        := ( chk9000.Checked );

  lblEvento.Visible       := cbEvento.Visible;
end;

end.


