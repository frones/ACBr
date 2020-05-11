unit Frm_ACBrEDI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, ACBrEDIOcorrencia, ACBrEDINotaFiscal, ACBrEDICobranca, IniFiles,
  ACBrEDIConhectos, ACBrEDIPreFatura, FileCtrl ;

type
  TfrmACBrEDI = class(TForm)
    pnlMenus: TPanel;
    pnlCentral: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PageControl4: TPageControl;
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
    sbPathOCOR: TSpeedButton;
    Label35: TLabel;
    edtPathOCOR: TEdit;
    btnSalvarConfig: TBitBtn;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    pgcBotoes: TPageControl;
    pgRespostas: TPageControl;
    Dados: TTabSheet;
    MemoDados: TMemo;
    tsOperacao: TTabSheet;
    edtPathCONEMB: TEdit;
    Label1: TLabel;
    edtPathDOCCOB: TEdit;
    Label2: TLabel;
    SpeedButton2: TSpeedButton;
    edtPathNOTAFIS: TEdit;
    Label3: TLabel;
    SpeedButton3: TSpeedButton;
    edtPathPREFAT: TEdit;
    Label4: TLabel;
    SpeedButton4: TSpeedButton;
    sbPathCONEMB: TSpeedButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    rgVersao: TRadioGroup;
    CONEMB: TACBrEDIConhectos;
    DOCCOB: TACBrEDICobranca;
    NOTAFIS: TACBrEDINotaFiscais;
    OCOR: TACBrEDIOcorrencia;
    PREFAT: TACBrEDIPreFatura;
    procedure sbPathOCORClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure sbPathCONEMBClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmACBrEDI: TfrmACBrEDI;

implementation

uses strutils, TypInfo, DateUtils, Grids, pediConversao, pcnAuxiliar,
     ACBrUtil ;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrEDI }

procedure TfrmACBrEDI.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrEDI.Button10Click(Sender: TObject);
begin
  PreFat.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: PreFat.Versao := ve30 ;
    1: PreFat.Versao := ve30a ;
    2: PreFat.Versao := ve31 ;
    3: PreFat.Versao := ve50 ;
  end;
  with PreFat.Cabecalho do   // informação obrigatória
  begin
    IdRegistro   := '000' ;
    Remetente    := edtEmitRazao.Text ;
    Destinatario := 'DESTINO PREFATURA' ;
    Data         := now ;
    Hora         := Time ;
    ID           := '' ;      // se não preenchido será informado no formato sugerido
    Sequencia    := 0 ;
    Filler       := '' ;      // preencher somente se tiver informações extras no registro
  end;
  with PreFat.InfoDocto.New do   // informação obrigatória
  begin
    IdRegistro := '390' ;     // Informar a Identificação do Registro no EDI conforme a versão
    IdDocto    := '' ;        // se não preenchido será informado no formato sugerido
    Filler     := '' ;
    with InfoPagadora.New do    // informação obrigatória
    begin
      IdRegistro := '391' ;
      CNPJ       := edtEmitCNPJ.Text ;
      Razao      := edtEmitRazao.Text ;
      Filler     := '' ;
      with InfoPreFatura.New do
      begin
        IdRegistro    := '392' ;
        IdPreFatura   := '' ;
        dtEmissao     := date ;
        dtPagamento   := date + 20 ;
        qtdeDoctos    := 1 ;
        vPreFatura    := 1000 ;
        Acao          := 'I' ;
        Filler        := '' ;
        with DoctosLiberados.New do
        begin
          IdRegistro       := '393' ;
          CnpjEmissor      := '99.999.999/9999-99' ;
          SerieDocto       := '1' ;
          IdDocto          := '124536' ;
          dtEmissao        := now ;
          SerieCTe         := '1' ;
          nroCTe           := '254130' ;
          dtEmissaoCTe     := now ;
          CnpjOrigem       := '99.999.999/9999-99' ;
          CnpjDestino      := '99.999.999/9999-99' ;
          tpCnpjDestino    := 0 ;
          vFreteEmbarcador := 1000 ;
          vFreteTransporte := 1000 ;
          tpDiferenca      := 0 ;
          vDiferenca       := 0 ;
          DoctoInterno     := '' ;
          Filler           := '' ;
          with CalculoFrete do
          begin
            IdRegistro    := '394' ;
            qPesoBruto    := 1000 ;
            vTotFretePeso := 1000 ;
            vAdValorem    := 0 ;
            vSecCat       := 0 ;
            vItrGris      := 0 ;
            vDespacho     := 150 ;
            vPedagio      := 50 ;
            vAdemeGris    := 0 ;
            vDiversos     := 0 ;
            vBCIcms       := 1000 ;
            pAliqIcms     := 12 ;
            vIcms         := 120 ;
            pAliqISS      := 0 ;
            vISS          := 0 ;
            vDesconto     := 0 ;
            Filler        := '' ;
          end;
        end;
        with NotasCTe.New do
        begin
          IdRegistro     := '396'  ;

          with InfoNotas.New do
          begin
            SerieNF  := '1' ;
            NumeroNF := 12345 ;
          end;
          with InfoNotas.New do
          begin
            SerieNF  := '1' ;
            NumeroNF := 24563 ;
          end;

          Filler     := '' ;
        end;
      end;
    end;
    with TotalPreFat do
    begin
      IdRegistro := '399' ;
      qPreFatura := 1 ;
      vTotal     := 1000 ;
      Filler     := '' ;
    end;
  end;
  PreFat.GravarArquivo(edtPathPreFat.Text+'\PREFATURA.TXT');

  MemoDados.Lines.Clear ;
  MemoDados.Lines.LoadFromFile(edtPathPreFat.Text+'\PREFATURA.TXT');
end;

procedure TfrmACBrEDI.Button1Click(Sender: TObject);
begin
  Ocor.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: Ocor.Versao := ve30 ;
    1: Ocor.Versao := ve30a ;
    2: Ocor.Versao := ve31 ;
    3: Ocor.Versao := ve50 ;
  end;

  with Ocor.Cabecalho do   // informação obrigatória
  begin
    IdRegistro   := '000' ;
    Remetente    := edtEmitRazao.Text ;
    Destinatario := 'DESTINO OCORRENCIA' ;
    Data         := now ;
    Hora         := Time ;
    ID           := '' ;      // se não preenchido será informado no formato sugerido
    Sequencia    := 0 ;
    Filler       := '' ;      // preencher somente se tiver informações extras no registro
  end;
  with Ocor.InfoOcor.New do   // informação obrigatória
  begin
     // Informar a Identificação do Registro no EDI conforme a versão
    IdRegistro := iif( Ocor.Versao = ve50, '540', '340') ;
    IdDocto    := '' ;        // se não preenchido será informado no formato sugerido
    Filler     := '' ;
    with Transportadora do    // informação obrigatória
    begin
      IdRegistro := iif( Ocor.Versao = ve50, '541', '341') ;
      CNPJ       := edtEmitCNPJ.Text ;
      Razao      := edtEmitRazao.Text ;
      Filler     := '' ;
      with OcorEntrega.New do      // informação obrigatória
      begin
        IdRegistro       := iif( Ocor.Versao = ve50, '542', '342') ;
        CNPJEmissorNF    := '99.999.999/9999-99' ;
        SerieNF          := '1' ;
        nNF              := 12345 ;
        cOcorrencia      := '001' ;
        dtOcorrencia     := date ;
        hrOcorrencia     := time ;
        cObsOcorrencia   := 1 ;
        FilialEmissorCT  := copy(edtEmitRazao.Text,1,10) ;
        SerieCT          := '1' ;
        nCT              := '1234' ;
        indTipoEntrega   := 1 ;
        codEmissorNF     := '' ;
        cFilialEmissorNF := '' ;
        Romaneio         := '' ;
        NumeroSAP1       := '' ;
        NumeroSAP2       := '' ;
        NumeroSAP3       := '' ;
        dtChegadaDestino := 0 ;
        hrChegadaDestino := 0 ;
        dtInicioDescarga := 0 ;
        hrInicioDescarga := 0 ;
        dtTerminoDescarga:= 0 ;
        hrTerminoDescarga:= 0 ;
        dtSaidaDestino   := 0 ;
        hrSaidaDestino   := 0 ;
        CNPJDevolucao    := '' ;
        SerieNFDevolucao := '' ;
        nNFDevolucao     := '' ;
        TextoLivre       := '' ;
        Filler           := '' ;
        with InfoCompl do       // Informar apenas se existir Versão 5.0 Condicional
        begin
          IdRegistro := '543' ;
          xMotivo1   := '12312' ;
          xMotivo2   := '' ;
          xMotivo3   := '' ;
          Filler     := '' ;
        end;
        with InfoItemNF.New do  // Informar apenas se existir Versão 5.0  Condicional
        begin
          IdRegistro := '544' ;
          cItem      := '1' ;
          xDescricao := 'ITEM TESTE' ;
          qVolumeNF  := 5 ;
          qVolEntreg := 5 ;
          Filler     := '' ;
        end;
        with Redespacho do  // informação condicional para Versão 5.0
        begin
          IdRegistro      := '545' ;
          CNPJContratante := '99.999.999/9999-99' ;
          CNPJEmissor     := edtEmitCNPJ.Text ;   // Apenas versão 5.0
          FilialEmissor   := copy(edtEmitRazao.Text,1,10) ;
          Serie           := '1' ;
          nCTe            := '25412' ;
          Filler          := '' ;
        end;
      end;
    end;
    with TotOcorrencias do   // apenas versão 5.0  (total de ocorrências) Obrigatória
    begin
      IdRegistro := '549';
      nQtde      := 1 ;
      Filler     := '' ;
    end;
  end;
  Ocor.GravarArquivo(edtPathOCor.Text+'\OCORRENCIA.TXT');

  MemoDados.Lines.Clear ;
  MemoDados.Lines.LoadFromFile(edtPathOCor.Text+'\OCORRENCIA.TXT');
end;

procedure TfrmACBrEDI.Button2Click(Sender: TObject);
var
  o, e: Integer ;
begin
  Ocor.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: Ocor.Versao := ve30 ;
    1: Ocor.Versao := ve30a ;
    2: Ocor.Versao := ve31 ;
    3: Ocor.Versao := ve50 ;
  end;
  Ocor.LerArquivo(edtPathOCor.Text+'\OCORRENCIA.TXT');
  MemoDados.Lines.Clear ;
  with Ocor.Cabecalho do   // informação obrigatória
  begin
    MemoDados.Lines.Add( IdRegistro + ' - ' + Remetente + ' - ' + Destinatario +
                         ID + ' - ' + IntToStr(Sequencia) ) ;
  end;
  for o := 0 to Ocor.InfoOcor.Count - 1 do   // informação obrigatória
  begin
    with Ocor.InfoOcor.Items[o] do
    begin
      MemoDados.Lines.Add( IdRegistro + ' - ' + IdDocto + ' - ' ) ;
      with Transportadora do    // informação obrigatória
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + CNPJ + ' - ' + Razao ) ;
        for e := 0 to OcorEntrega.Count - 1 do
        begin
          with OcorEntrega.Items[e] do      // informação obrigatória
          begin
            MemoDados.Lines.Add( IdRegistro + ' - ' + CNPJEmissorNF + ' - ' +
                SerieNF + ' - ' + IntToStr(nNF) + ' - ' + cOcorrencia + ' - ' +
                DateToStr(dtOcorrencia) + ' - ' + TimeToStr(hrOcorrencia) + ' - ' +
                FilialEmissorCT + ' - ' + SerieCT + ' - ' + nCT ) ;
          end;
        end;
      end;
      with TotOcorrencias do   // apenas versão 5.0  (total de ocorrências) Obrigatória
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + IntToStr(nQtde) ) ;
      end;
    end;
  end;
end;

procedure TfrmACBrEDI.Button3Click(Sender: TObject);
var
  o, e: Integer ;
begin
  ConEmb.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: ConEmb.Versao := ve30 ;
    1: ConEmb.Versao := ve30a ;
    2: ConEmb.Versao := ve31 ;
    3: ConEmb.Versao := ve50 ;
  end;
  ConEmb.LerArquivo(edtPathCONEMB.Text+'\CONHECTOS.TXT');
  MemoDados.Lines.Clear ;
  with ConEmb.Cabecalho do   // informação obrigatória
  begin
    MemoDados.Lines.Add( IdRegistro + ' - ' + Remetente + ' - ' + Destinatario +
                         ID + ' - ' + IntToStr(Sequencia) ) ;
  end;
  for o := 0 to ConEmb.InfoConEmb.Count - 1 do   // informação obrigatória
  begin
    with ConEmb.InfoConEmb.Items[o] do
    begin
      MemoDados.Lines.Add( IdRegistro + ' - ' + IdDocto + ' - ' ) ;
      with Transportadora do    // informação obrigatória
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + CNPJ + ' - ' + Razao ) ;
        for e := 0 to ConhectoEmbarcado.Count - 1 do
        begin
          with ConhectoEmbarcado.Items[e] do      // informação obrigatória
          begin
            MemoDados.Lines.Add( IdRegistro + ' - ' + Filial + ' - ' +
                Serie + ' - ' + nCTe + ' - ' + CNPJEmissor + ' - ' +
                CNPJEmbarq + ' - ' + PlacaVeiculo ) ;
          end;
        end;
      end;
      with TotConEmb do   // apenas versão 5.0  (total de ocorrências) Obrigatória
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + IntToStr(nQtde) + ' - ' +
                             FloatToStr(vTotal) ) ;
      end;
    end;
  end;
end;

procedure TfrmACBrEDI.Button4Click(Sender: TObject);
begin
  ConEmb.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: ConEmb.Versao := ve30 ;
    1: ConEmb.Versao := ve30a ;
    2: ConEmb.Versao := ve31 ;
    3: ConEmb.Versao := ve50 ;
  end;
  with ConEmb.Cabecalho do   // informação obrigatória
  begin
    IdRegistro   := '000' ;
    Remetente    := edtEmitRazao.Text ;
    Destinatario := 'DESTINO CONEMB' ;
    Data         := now ;
    Hora         := Time ;
    ID           := '' ;      // se não preenchido será informado no formato sugerido
    Sequencia    := 0 ;
    Filler       := '' ;      // preencher somente se tiver informações extras no registro
  end;
  with ConEmb.InfoConEmb.New do   // informação obrigatória
  begin
    IdRegistro := '520' ;     // Informar a Identificação do Registro no EDI conforme a versão
    IdDocto    := '' ;        // se não preenchido será informado no formato sugerido
    Filler     := '' ;
    with Transportadora do    // informação obrigatória
    begin
      IdRegistro := '521' ;
      CNPJ       := edtEmitCNPJ.Text ;
      Razao      := edtEmitRazao.Text ;
      Filler     := '' ;
      with ConhectoEmbarcado.New do
      begin
        IdRegistro        := '522' ;
        Filial            := copy(edtEmitRazao.Text,1,10) ;
        Serie             := '1' ;
        nCTe              := '1254' ;
        dtEmissao         := date ;
        tpFrete           := tcfFOB ;
        Continua          := 'U' ;
        CNPJEmissor       := edtEmitCNPJ.Text ;
        CNPJEmbarq        := '99.999.999/9999-99' ;
        Acao              := tpaInclusao;
        TipoCTe           := tpcNormalDeSaida ;
        CFOP              := '5-353' ;
        CNPJDevolucao     := '99.999.999/9999-99' ;
        CNPJConsignatario := '99.999.999/9999-99' ;
        CNPJDestinatario  := '99.999.999/9999-99' ;
        PlacaVeiculo      := 'SPAAA9999' ;
        Romaneio          := '' ;
        NumeroSAP1        := '' ;
        NumeroSAP2        := '' ;
        NumeroSAP3        := '' ;
        IdDocAutorizacao  := '' ;
        ChaveCTe          := '12345678901234567890123456789012345678901234' ;
        ProtocoloCTe      := '123456789012345' ;
        cCTe              := '123456789' ;
        MeioTransporte    := tmTocoFechado ;
        TranspContratante := 'teste' ;
        SerieContratante  := '1' ;
        CTeContratante    := '123' ;
        TipoFrete         := tpfNormal ;
        FreteDiferenciado := tediNao ;
        TabelaFrete       := '' ;
        CargaRapida       := tediNao;
        UFEmbarcador      := 'SP' ;
        UFEmissorCTe      := 'SP' ;
        UFDestinatario    := 'SP' ;
        Filler     := '' ;
        with ValoresConhecto do
        begin
          IdRegistro     := '523' ;
          qTotVolumes    := 1 ;
          qTotPesoBruto  := 10 ;
          qTotPesoCubado := 3 ;
          qPesoDensidade := 5 ;
          vTotFretePeso  := 1500 ;
          vTotFrete      := 600 ;
          vBcIcms        := 600 ;
          pAliqIcms      := 12 ;
          vIcms          := 72 ;
          vFrete         := 0 ;
          vSecCat        := 0 ;
          vITR           := 0 ;
          vDespacho      := 0 ;
          vPedagio       := 0 ;
          vAdemeGris     := 0 ;
          ST             := tstNao ;
          IDescAcrescimo := tdaNormal ;
          DireitoFiscal  := tdfSemST ;
          TipoImposto    := tpiSemST ;
          Filler         := '' ;
        end;
        with NotasConEmb.New do
        begin
          IdRegistro     := '524' ;
          qVolume        := 2 ;
          qPesoCubado    := 0 ;
          qPesoDensidade := 0 ;
          IdPedido       := '' ;
          TipoNF         := 1 ;
          Bonificacao    := tediNao ;
          CFOP           := '5101' ;
          UFGerador      := 'SP' ;
          Desdobro       := '' ;
          xSerie         := '1' ;
          xNumero        := '12543' ;
          CNPJEmissor    := '99.999.999/9999-99' ;
          dtEmissao      := date ;
          vNF            := 1500 ;
          qPesoNF        := 1500 ;
          Devolucao      := tediNao ;
          Filler         := '' ;
        end;
        with InfoEntrega.New do
        begin
          IdRegistro     := '525' ;
          CNPJEmissorNF1 := '99.999.999/9999-99' ;
          NomeEmissorNF1 := 'RAZAO TESTE' ;
          SerieNF1       := '1' ;
          NumeroNF1      := 2514 ;
          Filler         := '' ;
        end;
        with Consignatario do
        begin
          IdRegistro := '527' ;
          Razao      := 'Razao Teste' ;
          CNPJ       := '99.999.999/9999-99' ;
          IE         := '999999999999' ;
          Endereco   := 'Endereco Teste' ;
          Bairro     := 'TESTE' ;
          Filler     := '' ;
        end;
      end;
    end;
    with TotConEmb do
    begin
      IdRegistro := '529' ;
      nQtde      := 1 ;
      vTotal     := 200 ;
      Filler     := '' ;
    end;
  end;
  ConEmb.GravarArquivo(edtPathCONEMB.Text+'\CONHECTOS.TXT');

  MemoDados.Lines.Clear ;
  MemoDados.Lines.LoadFromFile(edtPathCONEMB.Text+'\CONHECTOS.TXT');
end;

procedure TfrmACBrEDI.Button5Click(Sender: TObject);
var
  o, e: Integer ;
begin
  DocCob.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: DocCob.Versao := ve30 ;
    1: DocCob.Versao := ve30a ;
    2: DocCob.Versao := ve31 ;
    3: DocCob.Versao := ve50 ;
  end;
  DocCob.LerArquivo(edtPathDOCCOB.Text+'\COBRANCA.TXT');
  MemoDados.Lines.Clear ;
  with DocCob.Cabecalho do   // informação obrigatória
  begin
    MemoDados.Lines.Add( IdRegistro + ' - ' + Remetente + ' - ' + Destinatario +
                         ID + ' - ' + IntToStr(Sequencia) ) ;
  end;
  for o := 0 to DocCob.InfoDocCob.Count - 1 do   // informação obrigatória
  begin
    with DocCob.InfoDocCob.Items[o] do
    begin
      MemoDados.Lines.Add( IdRegistro + ' - ' + IdDocto + ' - ' ) ;
      with Transportadora do    // informação obrigatória
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + CNPJ + ' - ' + Razao ) ;
        for e := 0 to DoctoCobranca.Count - 1 do
        begin
          with DoctoCobranca.Items[e] do      // informação obrigatória
          begin
            MemoDados.Lines.Add( IdRegistro + ' - ' + EmissorDocto + ' - ' +
                SerieDocto + ' - ' + NumeroDocto + ' - ' + DateToStr(dtFatura) + ' - ' +
                DateToStr(dtVencimento) + ' - ' + FloatToStr(vDocto) ) ;
          end;
        end;
      end;
      with TotCobranca do
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + IntToStr(nQtde) + ' - ' +
                             FloatToStr(vTotal) ) ;
      end;
    end;
  end;
end;

procedure TfrmACBrEDI.Button6Click(Sender: TObject);
begin
  DocCob.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: DocCob.Versao := ve30 ;
    1: DocCob.Versao := ve30a ;
    2: DocCob.Versao := ve31 ;
    3: DocCob.Versao := ve50 ;
  end;
  with DocCob.Cabecalho do   // informação obrigatória
  begin
    IdRegistro   := '000' ;
    Remetente    := edtEmitRazao.Text ;
    Destinatario := 'DESTINO COBRANCA' ;
    Data         := now ;
    Hora         := Time ;
    ID           := '' ;      // se não preenchido será informado no formato sugerido
    Sequencia    := 0 ;
    Filler       := '' ;      // preencher somente se tiver informações extras no registro
  end;
  with DocCob.InfoDocCob.New do   // informação obrigatória
  begin
    IdRegistro := '550' ;     // Informar a Identificação do Registro no EDI conforme a versão
    IdDocto    := '' ;        // se não preenchido será informado no formato sugerido
    Filler     := '' ;
    with Transportadora do    // informação obrigatória
    begin
      IdRegistro := '551' ;
      CNPJ       := edtEmitCNPJ.Text ;
      Razao      := edtEmitRazao.Text ;
      Filler     := '' ;
      with DoctoCobranca.New do
      begin
        IdRegistro    := '552' ;
        EmissorDocto  := copy(edtEmitRazao.Text, 1, 10) ;
        TipoDocto     := tdcNotaFiscal ;     // usado na versão 5.0
        SerieDocto    := '1' ;
        NumeroDocto   := '12345' ;
        dtFatura      := date ;    // na versão 3 data de Emissão
        dtVencimento  := date + 20 ;
        vDocto        := 1000 ;
        vICMS         := 0 ;
        TipoCobranca  := tcBanco ;
        pMulta        := 0 ;
        vJurosDiario  := 0 ;
        dtLimetePag   := date + 21 ;
        vDescto       := 0 ;
        cBanco        := 341 ;
        xBanco        := 'BANCO ITAU SA' ;
        nAgencia      := 1234 ;
        dvAgencia     := 'X' ;
        nConta        := 45621 ;
        dvConta       := '6' ;
        Acao          := tpaInclusao ;
        //  Tags usadas na versão 5.0
        idPreFatura   := 0 ;
        idComplFatura := '' ;
        CFOP          := '5353' ;
        cNFe          := 123456789 ;
        chNFe         := '12345678901234567890123456789012345678901234' ;
        xProtocoloNFe := '123456789012345' ;
        Filler        := '' ;
        with Impostos do
        begin
          IdRegistro  := '553' ;
          vBCIcms     := 0 ;
          pAliqIcms   := 12 ;
          vIcms       := 0 ;
          ST          := tstNao ;
          vBCIcmsST   := 0 ;
          pAliqIcmsST := 0 ;
          vIcmsST     := 0 ;
          vBcISS      := 0 ;
          pAliqISS    := 0 ;
          vISS        := 0 ;
          vIR         := 0 ;
          Filler      := '' ;
        end;
        with Conhectos.New do
        begin
          IdRegistro     := '555'  ;
          EmissorCTe     := copy(edtEmitRazao.Text, 1, 10) ;
          SerieCTe       := '1' ;
          nCTe           := '12' ;
          dtEmissao      := date ;
          vFrete         := 1000 ;
          Romaneio       := '' ;
          NumeroSAP1     := '' ;
          NumeroSAP2     := '' ;
          NumeroSAP3     := '' ;
          UFEmbarcador   := 'SP' ;
          UFEmissorCTe   := 'SP' ;
          UFDestinatario := 'SP' ;
          ContaRazao     := '422619' ;
          cIva           := 'Z3' ;
          CTDevolucao    := tediNao ;
          Filler     := '' ;
        end;
        with NotasFiscais.New do
        begin
          IdRegistro  := '556'  ;
          xSerie      := '1' ;
          xNumero     := '12' ;
          dtEmissao   := date ;
          qPesoNF     := 1000 ;
          vNF         := 2000 ;
          CNPJEmissor := '99.999.999/9999-99' ;
          Romaneio    := '1232131' ;
          NumeroSAP1  := '1245' ;
          NumeroSAP2  := '' ;
          NumeroSAP3  := '' ;
          Devolucao   := tediSim ;
          Filler      := '' ;
        end;
      end;
      with TotCobranca do
      begin
        IdRegistro := '559' ;
        nQtde      := 1 ;
        vTotal     := 1000.50 ;
        Filler     := '' ;
      end;
    end;
  end;
  DocCob.GravarArquivo(edtPathDOCCOB.Text+'\COBRANCA.TXT');

  MemoDados.Lines.Clear ;
  MemoDados.Lines.LoadFromFile(edtPathDOCCOB.Text+'\COBRANCA.TXT');
end;

procedure TfrmACBrEDI.Button7Click(Sender: TObject);
var
  e, d, i, n: Integer ;
begin
  NotaFis.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: NotaFis.Versao := ve30 ;
    1: NotaFis.Versao := ve30a ;
    2: NotaFis.Versao := ve31 ;
    3: NotaFis.Versao := ve50 ;
  end;
  NotaFis.LerArquivo( edtPathNOTAFIS.Text+'\NOTAFIS.TXT')  ;
  MemoDados.Lines.Clear ;
  with NotaFis.Cabecalho do
  begin
    MemoDados.Lines.Add(IdRegistro+' - '+Remetente) ;
  end ;

  for d := 0 to NotaFis.InfoDocto.Count - 1 do
  begin
    with NotaFis.InfoDocto.Items[d] do
    begin
      MemoDados.Lines.Add(IdRegistro+' - '+IdDocto) ;

      for e := 0 to InfoEmbarcadora.Count - 1 do
      begin
        with InfoEmbarcadora.Items[e] do
        begin
          MemoDados.Lines.Add(IdRegistro+' - '+Razao) ;
          for i := 0 to InfoDestinatario.Count - 1 do
          begin
            with InfoDestinatario.Items[i] do
            begin
              MemoDados.Lines.Add( IdRegistro+' - '+Razao) ;
              for n := 0 to InfoNotas.Count - 1 do
              begin
                with InfoNotas.Items[n] do
                begin
                  MemoDados.Lines.Add( IdRegistro+' - '+chNFe ) ;
                  MemoDados.Lines.Add(ValoresNF.IdRegistro+' - '+FormatFloat('#0.00', ValoresNF.vTotalNF)) ;
                end;
              end;
            end;
          end;
        end;
      end;
      with TotalNotas do
         MemoDados.Lines.Add(IdRegistro+' - '+FormatFloat('#0.00', vTotal)+' - '+
                                              FormatFloat('#0.00', vCobrado)+' - '+
                                              FormatFloat('#0.00', qVolumes)+' - '+
                                              FormatFloat('0000', qNotas)) ;
    end;
  end;
end;

procedure TfrmACBrEDI.Button8Click(Sender: TObject);
begin
  NotaFis.LimpaRegistros ;
  // informar => ve30 -> 3.0, ve30a -> 3.0a, ve31 -> 3.01 ou ve50 -> 5.0
  case rgVersao.ItemIndex of
    0: NotaFis.Versao := ve30 ;
    1: NotaFis.Versao := ve30a ;
    2: NotaFis.Versao := ve31 ;
    3: NotaFis.Versao := ve50 ;
  end;

  with NotaFis.Cabecalho do   // informação obrigatória
  begin
    IdRegistro   := '000' ;
    Remetente    := edtEmitRazao.Text ;
    Destinatario := 'DESTINO NOTA FISCAL' ;
    Data         := now ;
    Hora         := Time ;
    ID           := '' ;      // se não preenchido será informado no formato sugerido
    Sequencia    := 0 ;
    Filler       := '' ;      // preencher somente se tiver informações extras no registro
  end;
  with NotaFis.InfoDocto.New do   // informação obrigatória
  begin
    IdRegistro := '500' ;     // Informar a Identificação do Registro no EDI conforme a versão
    IdDocto    := '' ;        // se não preenchido será informado no formato sugerido
    Filler     := '' ;
    with InfoEmbarcadora.New do    // informação obrigatória
    begin
      IdRegistro  := '501' ;
      Razao       := edtEmitRazao.Text ;
      CNPJCPF     := edtEmitCNPJ.Text ;
      IE          := edtEmitIE.Text ;
      IEST        := edtEmitIE.Text ;
      IM          := '' ;
      Endereco    := edtEmitLogradouro.Text+' '+edtEmitNumero.Text+' '+ edtEmitComp.Text ;
      Bairro      := edtEmitBairro.Text ;
      Cidade      := edtEmitCidade.Text ;
      cMunicipio  := edtEmitCodCidade.Text ;
      Cep         := edtEmitCep.Text ;
      UF          := edtEmitUF.Text ;
      dtEmbarque  := now ;
      AreaFrete   := '' ;
      Telefone    := 'TESTE 11 91234-5678' ;
      Filler      := '' ;
      with InfoDestinatario.New do
      begin
        IdRegistro  := '503' ;
        Razao       := 'DESTINATARIO' ;
        CNPJCPF     := '99.999.999/9999-99' ;
        IE          := '999999999999' ;
        InsSuframa  := '' ;
        Endereco    := 'RUA TESTE 12'  ;
        Bairro      := 'TESTE' ;
        Cidade      := 'JUNDIAÍ' ;
        cMunicipio  := '3525904' ;
        Cep         := '13203001' ;
        UF          := 'SP' ;
        Telefone    := '11 91234-5678' ;
        cPais       := '1058' ;
        AreaFrete   := '' ;
        Tipo        := '1' ;
        TpComercio  := 'C' ;
        Filler      := '' ;
        with InfoNotas.New do
        begin
          IdRegistro      := '505' ;
          xSerie          := '1' ;
          xNumero         := '12345' ;
          dtEmissao       := StrToDate('31/01/2020') ;
          tpMercadoria    := 'ALIMENTOS' ;
          xEspecie        := 'CAIXAS' ;
          cRota           := '' ;
          MeioTransporte  := 1  ;
          TipoTransporte  := 1 ;
          TipoCarga       := 2 ;
          CondicaoFrete   := 'C' ;
          dtEmbarque      := now ;
          Desdobro        := '' ;
          CargaRapida     := tediNao ;
          TipoNF          := 1 ;
          Bonificacao     := tediNao ;
          CFOP            := '5102' ;
          UFGerador       := 'SP' ;
          FreteDifer      := tediNao ;
          TabelaFrete     := '' ;
          ModalidadeFrete := '05' ;
          IdPedido        := '' ;
          Romaneio        := '' ;
          NumeroSAP1      := '' ;
          NumeroSAP2      := '' ;
          NumeroSAP3      := '' ;
          TipoEntrega     := 1 ;
          dtInicio        := StrToDate('31/01/2020') ;
          hrInicio        := StrToTime('09:00') ;
          dtTermino       := 0 ;
          hrTermino       := 0 ;
          cNFe            := '123456789' ;
          chNFe           := '12345678901234567890123456789012345678901234' ;
          ProtocoloNFe    := '123456789012345' ;
          Acao            := tpaInclusao ;
          Filler          := '' ;

          with ValoresNF do
          begin
            IdRegistro     := '506' ;
            qVolumes       := 10 ;
            qPesoBruto     := 1050 ;
            qPesoLiquido   := 1000 ;
            qPesoCubado    := 0 ;
            qPesoDensidade := 0 ;
            IncideICMS     := ticNao ;
            SeguroEfetuado := tediNao ;
            vCobrado       := 100.00 ;
            vTotalNF       := 100.00 ;
            vTotalSeguro   := 0 ;
            vTotalDescto   := 0 ;
            vOutraDespesas := 0 ;
            vBCIcms        := 100.00 ;
            vIcms          := 12.00 ;
            vBCIcmsST      := 0 ;
            vIcmsST        := 0 ;
            vIcmsRetido    := 0 ;
            vImpImportacao := 0 ;
            vIPI           := 0 ;
            vPIS           := 0 ;
            vCofins        := 0 ;
            vFrete         := 0 ;
            vFretePeso     := 100 ;
            vAdValorem     := 0 ;
            vTaxas         := 0 ;
            vIcmsFrete     := 0 ;
            vIcmsFreteST   := 0 ;
            vIssFrete      := 0 ;
            Filler         := '' ;
          end;

          // Registro 333 Obrigatório Apenas para versão 3.1 ou anterior
          with ComplementoNF do
          begin
            IdRegistro     := '333' ;
            CFOP           := '5102' ;
            TipoEntrega    := 0 ;
            dtInicio       := date ;
            hrInicio       := Time ;
            dtTermino      := date + 5 ;
            hrTermino      := Time + 20 ;
            LocDesembarque := 'TESTE' ;
            FreteDifer     := tediNao ;
            TabelaFrete    := '' ;
            EmissorNF1     := '' ;
            SerieNF1       := '' ;
            NumeroNF1      := '' ;
            vDespesas      := 0 ;
            TipoVeiculo    := tmTocoFechado ;
            EmissorCTe     := 'MATRIZ' ;
            SerieCTe       := '1' ;
            NumeroCTE      := '120' ;
            Filler         := '' ;
          end;
        end;
      end;
    end;
    with TotalNotas do
    begin
      IdRegistro := '519' ;
      vTotal     := 100 ;
      vCobrado   := 100 ;
      qPesoBruto := 1050 ;
      qPesoDensi := 0 ;
      qVolumes   := 10 ;
      qNotas     := 1 ;
      vSeguro    := 0 ;
      Filler     := '' ;
    end;
  end;
  NotaFis.GravarArquivo(edtPathNOTAFIS.Text+'\NOTAFIS.TXT');

  MemoDados.Lines.Clear ;
  MemoDados.Lines.LoadFromFile(edtPathNOTAFIS.Text+'\NOTAFIS.TXT');
end;

procedure TfrmACBrEDI.Button9Click(Sender: TObject);
var
  e, i, p: integer ;
begin
  PreFat.LimpaRegistros ;

  case rgVersao.ItemIndex of
    0: PreFat.Versao := ve30 ;
    1: PreFat.Versao := ve30a ;
    2: PreFat.Versao := ve31 ;
    3: PreFat.Versao := ve50 ;
  end;

  PreFat.LerArquivo(edtPathPreFat.Text+'\PREFATURA.TXT');
  MemoDados.Lines.Clear ;
  with PreFat.Cabecalho do   // informação obrigatória
  begin
    MemoDados.Lines.Add( IdRegistro + ' - ' + Remetente + ' - ' + Destinatario +
                         ID + ' - ' + IntToStr(Sequencia) ) ;
  end;

  for i := 0 to PreFat.InfoDocto.Count - 1 do   // informação obrigatória
  begin
    with PreFat.InfoDocto.Items[i] do
    begin
      MemoDados.Lines.Add( IdRegistro + ' - ' + IdDocto + ' - ' ) ;
      with InfoPagadora.Items[p] do    // informação obrigatória
      begin
        MemoDados.Lines.Add( IdRegistro + ' - ' + CNPJ + ' - ' + Razao ) ;
        for e := 0 to InfoPreFatura.Count - 1 do
        begin
          with InfoPreFatura.Items[e] do      // informação obrigatória
          begin
            MemoDados.Lines.Add( IdRegistro + ' - ' + IdPreFatura + ' - ' +
                DateToStr(dtEmissao) + ' - ' + DateToStr(dtPagamento) + ' - ' +
                IntToStr(qtdeDoctos) + ' - ' + FloatToStr(vPreFatura) ) ;
          end;
        end;
        with TotalPreFat do
          MemoDados.Lines.Add( IdRegistro + ' - ' + IntToStr(qPreFatura) + ' - ' +
                             FloatToStr(vTotal) ) ;
      end;
    end;
  end;
end;

procedure TfrmACBrEDI.FormShow(Sender: TObject);
begin
  LerConfiguracao ;
end;

procedure TfrmACBrEDI.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrEDI.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrEDI.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrEDI.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrEDI.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrEDI.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrEDI.PathClick(Sender: TObject);
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

procedure TfrmACBrEDI.sbPathCONEMBClick(Sender: TObject);
begin
  PathClick(edtPathCONEMB);
end;

procedure TfrmACBrEDI.sbPathOCORClick(Sender: TObject);
begin
  PathClick(edtPathOCOR);
end;

procedure TfrmACBrEDI.SpeedButton2Click(Sender: TObject);
begin
  PathClick(edtPathDOCCOB);
end;

procedure TfrmACBrEDI.SpeedButton3Click(Sender: TObject);
begin
  PathClick(edtPathNOTAFIS);
end;

procedure TfrmACBrEDI.SpeedButton4Click(Sender: TObject);
begin
  PathClick(edtPathPREFAT);
end;

procedure TfrmACBrEDI.LerConfiguracao;
var
  Arq: String ;
  Ini: TIniFile ;
begin
  Arq := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(Arq);
  try
    rgVersao.ItemIndex     := Ini.ReadInteger('Arquivos', 'VersaoEDI'  ,  0);
    edtPathOCOR.Text       := Ini.ReadString ('Arquivos', 'PathOCOR'   , '');
    edtPathCONEMB.Text     := Ini.ReadString ('Arquivos', 'PathCONEMB' , '');
    edtPathDOCCOB.Text     := Ini.ReadString ('Arquivos', 'PathDOCCOB' , '');
    edtPathNOTAFIS.Text    := Ini.ReadString ('Arquivos', 'PathNOTAFIS', '');
    edtPathPREFAT.Text     := Ini.ReadString ('Arquivos', 'PathPREFAT' , '');

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

  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrEDI.GravarConfiguracao;
var
  Arq: String ;
  Ini: TIniFile ;
begin
  Arq := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(Arq);
  try
    Ini.WriteString ('Arquivos', 'PathOCOR'   ,  edtPathOCOR.Text);
    Ini.WriteString ('Arquivos', 'PathCONEMB' ,  edtPathCONEMB.Text);
    Ini.WriteString ('Arquivos', 'PathDOCCOB' ,  edtPathDOCCOB.Text);
    Ini.WriteString ('Arquivos', 'PathNOTAFIS',  edtPathNOTAFIS.Text);
    Ini.WriteString ('Arquivos', 'PathPREFAT' ,  edtPathPREFAT.Text);
    Ini.WriteInteger('Arquivos', 'VersaoEDI'  ,  rgVersao.ItemIndex);

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

  finally
    Ini.Free;
  end;
  LerConfiguracao ;
end;

end.
