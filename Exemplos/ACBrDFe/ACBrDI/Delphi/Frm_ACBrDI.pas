unit Frm_ACBrDI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, XMLDoc,
  ACBrDIDeclaracaoImportacao, pcnDI, FileCtrl, ComCtrls;

type
  TfrmACBrDI = class(TForm)
    BtnLerXMLIndividualDI: TBitBtn;
    MemoLogs: TMemo;
    OpenDialog1: TOpenDialog;
    BtnLerMultiplosXML: TBitBtn;
    PanelTop: TPanel;
    Shape1: TShape;
    MemoXML: TMemo;
    BtnLimpar: TBitBtn;
    PageControlPrincipal: TPageControl;
    TabXMLDI: TTabSheet;
    TabLogs: TTabSheet;
    procedure BtnLerXMLIndividualDIClick(Sender: TObject);
    procedure BtnLimparClick(Sender: TObject);
    procedure BtnLerMultiplosXMLClick(Sender: TObject);
  private
    procedure ExibirInformacoesDeclaracaoEmMemo(DI: TDI);overload;
    procedure ExibirInformacoesDeclaracaoEmMemo(DeclaracoesImportacao: TDeclaracoesImportacao);overload;
    procedure Limpar;
  public
    { Public declarations }
  end;

var
  frmACBrDI: TfrmACBrDI;

implementation

uses
  ACBrUtil.Strings;

{$R *.dfm}

procedure TfrmACBrDI.BtnLerMultiplosXMLClick(Sender: TObject);
const
  SELDIRHELP = 1000;
var
  Dir: String;
  SR: TSearchRec;
  sXML : TStrings;
  DeclaracoesImportacao: TDeclaracoesImportacao;
begin
  Dir := OpenDialog1.FileName;
  if(SelectDirectory(Dir,[sdPrompt], SELDIRHELP))then
  begin
  // AQUI FAÇO A LEITURA DE UM MÚLTIPLOS XMLS ADICIONANDO NA TDECLARACOESIMPORTACAO
  {===============ATENÇÃO=======================================================
   Esta classe serve para ler o XML da DI que tem a seguinte estrutura.
    <ListaDeclaracoes>
      <declaracaoImportacao>
   ============================================================================}
    sXML := TStringList.Create;
    DeclaracoesImportacao := TDeclaracoesImportacao.Create;
    try
      Limpar;
      try
        if(FindFirst(Dir+'\*.xml', faAnyFile, SR) = 0)then
        begin
          repeat
            if(SR.Attr <> faDirectory)then
            begin
              sXML.Clear;
              try
                sXML.LoadFromFile(Dir+'\'+SR.Name, TEncoding.UTF8);
              except
                on E:Exception do
                  sXML.LoadFromFile(Dir+'\'+SR.Name);
              end;
              MemoXML.Lines.Add(sXML.Text);
              MemoXML.Lines.Add('=====================================================');
              //Usei LoadFromFile, mas a classe tambem fornece LoadFromStream e LoadFromString
              DeclaracoesImportacao.LoadFromFile(Dir+'\'+SR.Name);
            end;
          until FindNext(SR) <> 0;
        end;
        ExibirInformacoesDeclaracaoEmMemo(DeclaracoesImportacao);
      except
        on E:Exception do
        begin
          MemoLogs.Lines.Add('ERRO: ' + E.Message);
        end;
      end;
    finally
      DeclaracoesImportacao.Free;
      sXML.Free;
    end;
  end;
end;

procedure TfrmACBrDI.BtnLerXMLIndividualDIClick(Sender: TObject);
var
  DecImportacao: TDeclaracaoImportacao;
  DI: TDI;
  sXML : TStrings;
  aFilePath: String;
begin
  if(OpenDialog1.Execute)then
  begin
  // AQUI FAÇO A LEITURA DE UM XML INDIVIDUAL DIRETAMENTE NA CLASSE TDECLARACAOIMPORTACAO
  {===============ATENÇÃO=======================================================
   Esta classe serve para ler o XML da DI que tem a seguinte estrutura.
    <ListaDeclaracoes>
      <declaracaoImportacao>
   ============================================================================}

    sXML := TStringList.Create;
    DecImportacao := TDeclaracaoImportacao.Create;
    try
      //Preciso pegar conteúdo do XML para passar ao método.
      try
        sXML.LoadFromFile(OpenDialog1.FileName, TEncoding.UTF8);
      except
        on E:Exception do
        begin
          sXML.LoadFromFile(OpenDialog1.FileName);
        end;
      end;
      if(Trim(sXML.Text) = '')then
      begin
        MemoLogs.Lines.Add('ERRO: Arquivo XML vazio!');
        exit;
      end;
      Limpar;
      try
        MemoXML.Text := sXML.Text;
        if(DecImportacao.LerXML(sXML.Text))then //LerXML é uma function que retorna um boolean
        begin
          DI := DecImportacao.DI;
          ExibirInformacoesDeclaracaoEmMemo(DI);
        end;
      except
        on E:Exception do
        begin
          MemoLogs.Lines.Add('ERRO: ' + E.Message);
        end;
      end;

    finally
      DecImportacao.Free;
      sXML.Free;
    end;
  end;
end;

procedure TfrmACBrDI.BtnLimparClick(Sender: TObject);
begin
  Limpar;
end;

procedure TfrmACBrDI.ExibirInformacoesDeclaracaoEmMemo(DeclaracoesImportacao: TDeclaracoesImportacao);
var
  i: Integer;
begin
  for i:=0 to DeclaracoesImportacao.Count - 1 do
  begin
    ExibirInformacoesDeclaracaoEmMemo(DeclaracoesImportacao[i].DI);
    MemoLogs.Lines.Add('=====================================================');
  end;
end;

procedure TfrmACBrDI.Limpar;
begin
  MemoXML.Clear;
  Memologs.Clear;
end;

procedure TfrmACBrDI.ExibirInformacoesDeclaracaoEmMemo(DI: TDI);
var
  adiCount, acrCount, icmsCount, pagCount, deduCount, docCount, nomeAduCount, mercCount : Integer;
begin
  //Para a lista completa das propriedades disponíveis consulte a unit pcnDI.
  MemoLogs.Lines.Add('numeroDI => '                        + DI.numeroDI);
  MemoLogs.Lines.Add('dataDesembaracao => '                + FormatDateTime('DD/MM/YYYY', DI.dataDesembaraco));
  MemoLogs.Lines.Add('ImportadorCpfRepresentanteLegal => ' + DI.importadorCpfRepresentanteLegal);
  MemoLogs.Lines.Add('viaTransporteNome => '               + DI.viaTransporteNome);
  for adiCount := 0 to DI.adicao.Count -1 do
  begin
    MemoLogs.Lines.Add('adicao.numeroAdicao => '                    + IntToStr(DI.adicao[adiCount].numeroAdicao));
    MemoLogs.Lines.Add('adicao.cideValorRecolher => ' + FormatFloat('#,##0.00', DI.adicao[adiCount].cideValorRecolher));
    MemoLogs.Lines.Add('adicao.codigoVinculoCompradorVendedor => ' + IntToStr(DI.adicao[adiCount].codigoVinculoCompradorVendedor));
    MemoLogs.Lines.Add('adicao.cofinsAliquotaValorRecolher => ' + FormatFloat('#,##0.00', DI.adicao[adiCount].cofinsAliquotaValorRecolher));
    MemoLogs.Lines.Add('adicao.condicaoVendaLocal => ' + DI.adicao[adiCount].condicaoVendaLocal);
    MemoLogs.Lines.Add('adicao.condicaoVendaMetodoValoracaoNome => ' + DI.adicao[adiCount].condicaoVEndaMetodoValoracaoNome);
    MemoLogs.Lines.Add('adicao.condicaoVendaMoedaNome => ' + DI.adicao[adiCount].condicaoVendaMoedaNome);
    MemoLogs.Lines.Add('adicao.condicaoVendaValorMoeda => ' + FormatFloat('#,##0.00', DI.adicao[adiCount].condicaoVendaValorMoeda));
    MemoLogs.Lines.Add('adicao.condicaoVendaValorReais => ' + FormatFloat('#,##0.00', DI.adicao[adiCount].condicaoVendaValorReais));
    MemoLogs.Lines.Add('adicao.dadosCambiaisValorRealCambio =>' + FormatFloat('#,##0.00', DI.adicao[adiCount].dadosCambiaisValorRealCambio));
    MemoLogs.Lines.Add('adicao.dadosCargaPaisProcedenciaCodigo => ' + IntToStr(DI.adicao[adiCount].dadosCargaPaisProcedenciaCodigo));
    MemoLogs.Lines.Add('adicao.dadosCargaUrfEntradaCodigo => ' + IntToStr(DI.adicao[adiCount].dadosCargaUrfEntradaCodigo));
    MemoLogs.Lines.Add('adicao.dadosCargaViaTransporteCodigo => ' + IntToStr(DI.adicao[adiCount].dadosCargaViaTransporteCodigo));
    MemoLogs.Lines.Add('adicao.dadosMercadoriaCodigoNcm => ' + DI.adicao[adiCount].dadosMercadoriaCodigoNcm);
    MemoLogs.Lines.Add('adicao.dadosMercadoriaNomeNcm => '     + DI.adicao[adiCount].dadosMercadoriaNomeNcm);
    MemoLogs.Lines.Add('adicao.dadosMercadoriaPesoLiquido => ' + FormatFloat('#,##0.00', DI.adicao[adiCount].dadosMercadoriaPesoLiquido));
    MemoLogs.Lines.Add('adicao.dcrIdentificacao => '           + IntToStr(DI.adicao[adiCount].dcrIdentificacao));
    MemoLogs.Lines.Add('adicao.dcrValorDevido => '             + FormatFloat('#,##0.00', DI.adicao[adiCount].dcrValorDevido));
    MemoLogs.Lines.Add('adicao.dcrValorDolar => '              + FormatFloat('#,##0.00', DI.adicao[adiCount].dcrValorDolar));
    MemoLogs.Lines.Add('adicao.dcrValorReal => '               + FormatFloat('#,##0.00' , DI.adicao[adiCount].dcrValorReal));
    MemoLogs.Lines.Add('adicao.dcrValorRecolher => '           + FormatFloat('#,##0.00' , DI.adicao[adiCount].dcrValorRecolher));
    MemoLogs.Lines.Add('adicao.destaqueNcm.numeroDestaque => ' + IntToStr(DI.adicao[adiCount].destaqueNcm.numeroDestaque));
    MemoLogs.Lines.Add('adicao.fabricanteNome => '             + DI.adicao[adiCount].fabricanteNome);
    MemoLogs.Lines.Add('adicao.fabricanteCidade => '           + DI.adicao[adiCount].fabricanteCidade);
    MemoLogs.Lines.Add('adicao.fabricanteEstado => '           + DI.adicao[adiCount].fabricanteEstado);
    MemoLogs.Lines.Add('adicao.fornecedorNome => '             + DI.adicao[adiCount].fornecedorNome);
    MemoLogs.Lines.Add('adicao.fornecedorCidade => '           + DI.adicao[adiCount].fornecedorCidade);
    MemoLogs.Lines.Add('adicao.fornecedorEstado => '           + DI.adicao[adiCount].fornecedorEstado);
    MemoLogs.Lines.Add('adicao.freteValorMoedaNegociada => '   + FormatFloat('#,##0.00', DI.adicao[adiCount].freteValorMoedaNegociada));
    MemoLogs.Lines.Add('adicao.freteValorReais => '            + FormatFloat('#,##0.00', DI.adicao[adiCount].freteValorReais));
    MemoLogs.Lines.Add('adicao.iiAcordoTarifarioAladiNome => '   + DI.adicao[adiCount].iiAcordoTarifarioAladiNome);
    MemoLogs.Lines.Add('adicao.iiAcordoTarifarioAtoLegalNumero => ' + IntToStr(DI.adicao[adiCount].iiAcordoTarifarioAtoLegalNumero));
    MemoLogs.Lines.Add('adicao.iiAcordoTarifarioTipoNome => '             + DI.adicao[adiCount].iiAcordoTarifarioTipoNome);
    MemoLogs.Lines.Add('adicao.iiAliquotaPercentualReducao => '           + FormatFloat('#,##0.00', DI.adicao[adiCount].iiAliquotaPercentualReducao));
    MemoLogs.Lines.Add('adicao.iiAliquotaValorRecolher => '               + FormatFloat('#,##0.00', DI.adicao[adiCount].iiAliquotaValorRecolher));
    MemoLogs.Lines.Add('adicao.iiBaseCalculo => '                         + FormatFloat('#,##0.00', DI.adicao[adiCount].iiBaseCalculo));
    MemoLogs.Lines.Add('adicao.iiRegimeTributacaoNome => '                + DI.adicao[adiCount].iiRegimeTributacaoNome);
    MemoLogs.Lines.Add('adicao.ipiAliquotaNotaComplementarTIPI => ' + FormatFloat('#,##0.00', DI.adicao[adiCount].ipiAliquotaNotaComplementarTIPI));
    MemoLogs.Lines.Add('adicao.ipiAliquotaValorRecolher => '        + FormatFloat('#,##0.00', DI.adicao[adiCount].ipiAliquotaValorRecolher));
    MemoLogs.Lines.Add('adicao.ipiRegimeTributacaoNome => '         + DI.adicao[adiCount].ipiRegimeTributacaoNome);
    MemoLogs.Lines.Add('adicao.numeroDI => '                        + DI.adicao[adiCount].numeroDI);
    MemoLogs.Lines.Add('adicao.paisAquisicaoMercadoriaNome => '     + DI.adicao[adiCount].paisAquisicaoMercadoriaNome);
    MemoLogs.Lines.Add('adicao.paisOrigemMercadoriaNome => '        + DI.adicao[adiCount].paisOrigemMercadoriaNome);
    MemoLogs.Lines.Add('adicao.pisCofinsBaseCalculoValor => '                 + FormatFloat('#,##0.00', DI.adicao[adiCount].pisCofinsBaseCalculoValor));
    MemoLogs.Lines.Add('adicao.pisPasepAliquotaReduzida => '                  + FormatFloat('#,##0.00', DI.adicao[adiCount].pisPasepAliquotaReduzida));
    MemoLogs.Lines.Add('adicao.pisPasepAliquotaValorDevido => '               + FormatFloat('#,##0.00', DI.adicao[adiCount].pisPasepAliquotaValorDevido));
    MemoLogs.Lines.Add('adicao.pisPasepAliquotaValorRecolher => '             + FormatFloat('#,##0.00', DI.adicao[adiCount].pisPasepAliquotaValorRecolher));
    MemoLogs.Lines.Add('adicao.relacaoCompradorVendedor => '                  + DI.adicao[adiCount].relacaoCompradorVendedor);
    MemoLogs.Lines.Add('adicao.seguroValorMoedaNegociada => '                 + FormatFloat('#,##0.00', DI.adicao[adiCount].seguroValorMoedaNegociada));
    MemoLogs.Lines.Add('adicao.sequencialRetificacao => '                     + IntToStr(DI.adicao[adiCount].sequencialRetificacao));
    MemoLogs.Lines.Add('adicao.valorMultaARecolherAjustado => '               + FormatFloat('#,##0.00', DI.adicao[adiCount].valorMultaARecolherAjustado));
    MemoLogs.Lines.Add('adicao.valorReaisFreteInternacional => '              + FormatFloat('#,##0.00', DI.adicao[adiCount].valorReaisFreteInternacional));
    MemoLogs.Lines.Add('adicao.valorReaisSeguroInternacional => '             + FormatFloat('#,##0.00', DI.adicao[adiCount].valorReaisSeguroInternacional));
    MemoLogs.Lines.Add('adicao.valorTotalCondicaoVenda => '                   + Formatfloat('#,##0.00', DI.adicao[adiCount].valorTotalCondicaoVenda));
    MemoLogs.Lines.Add('adicao.vinculoCompradorVendedor => '                  + DI.adicao[adiCount].vinculoCompradorVendedor);

    for docCount := 0 to DI.adicao[adiCount].documentoVinculado.Count -1 do
    begin
      MemoLogs.Lines.Add('adicao.documentoVinculado.nomeTipo => '   + DI.adicao[adiCount].documentoVinculado[docCount].nomeTipo);
      MemoLogs.Lines.Add('adicao.documentoVinculado.numero => '     + DI.adicao[adiCount].documentoVinculado[docCount].numero);
    end;

    for mercCount := 0 to DI.adicao[adiCount].mercadoria.Count - 1 do
    begin
      MemoLogs.Lines.Add('adicao.mercadoria.numeroSequencialItem => ' + IntToStr(DI.adicao[adiCount].mercadoria[mercCount].numeroSequencialItem));
      MemoLogs.Lines.Add('adicao.mercadoria.descricaoMercadoria => '  + DI.adicao[adiCount].mercadoria[mercCount].descricaoMercadoria);
      MemoLogs.Lines.Add('adicao.mercadoria.unidadeMedida => '        + DI.adicao[adiCount].mercadoria[mercCount].unidadeMedida);
    end;

    for acrCount := 0 to DI.adicao[adiCount].acrescimo.Count - 1 do
    begin
      MemoLogs.Lines.Add('adicao.acrescimo.denominacao => '          + DI.adicao[adiCount].acrescimo[acrCount].denominacao);
      MemoLogs.Lines.Add('adicao.acrescimo.moedaNegociadaNome => '   + DI.adicao[adiCount].acrescimo[acrCount].moedaNegociadaNome);
      MemoLogs.Lines.Add('adicao.acrescimo.valorMoedaNegociada => '  + FormatFloat('#,##0.00', DI.adicao[adiCount].acrescimo[acrCount].valorMoedaNegociada));
    end;
    for deduCount := 0 to DI.adicao[adiCount].deducao.Count - 1 do
    begin
      MemoLogs.Lines.Add('adicao.deducao.denominacao => '          + DI.adicao[adiCount].deducao[deduCount].denominacao);
      MemoLogs.Lines.Add('adicao.deducao.moedaNegociadaNome => '   + DI.adicao[adiCount].deducao[deduCount].moedaNegociadaNome);
      MemoLogs.Lines.Add('adicao.deducao.valorMoedaNegociada => '  + FormatFloat('#,##0.00', DI.adicao[adiCount].deducao[deduCount].valorMoedaNegociada));
    end;
    for nomeAduCount := 0 to DI.adicao[adiCount].nomenclaturaValorAduaneiro.Count - 1 do
    begin
      MemoLogs.Lines.Add('adicao.nomenclaturaValorAduaneiro.nivelNome => ' + DI.adicao[adiCount].nomenclaturaValorAduaneiro[nomeAduCount].nivelNome);
    end;

  end;

  for icmsCount := 0 to DI.icms.Count - 1 do
  begin
    MemoLogs.Lines.Add('icms.dataPagamentoIcms => '          + FormatDateTime('DD/MM/YYYY', DI.icms[icmsCount].dataPagamentoIcms));
    MemoLogs.Lines.Add('icms.ufIcms => '                     + DI.icms[icmsCount].ufIcms);
    MemoLogs.Lines.Add('icms.valorTotalIcms => '             + FormatFloat('#,##0.00', DI.icms[icmsCount].valorTotalIcms));
  end;

end;

end.
