unit Form.NFSe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ACBrBase,
  ACBrDFe, ACBrNFSeX, ACBrSocket, ACBrIBGE;

type
  TFNFSe = class(TForm)
    G: TGroupBox;
    ckVariosItens: TCheckBox;
    rgTipoImpressao: TRadioGroup;
    Button1: TButton;
    rgStatus: TRadioGroup;
    gbLogomarca: TGroupBox;
    ckLogomarcaPrefeitura: TCheckBox;
    ckHomologacao: TCheckBox;
    ACBrNFSeX1: TACBrNFSeX;
    ckLogomarcaPrestador: TCheckBox;
    ckQRCode: TCheckBox;
    rbProvedor: TRadioGroup;
    ACBrIBGE1: TACBrIBGE;
    ckOutrasInformacoes: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure AlimentarNFSe(NumDFe, NumLote: String);
    procedure ConfiguraProvedorECarregaXMLExemplo;

    function RoundTo5(Valor: Double; Casas: Integer): Double;
  public
    { Public declarations }
  end;

var
  FNFSe: TFNFSe;

implementation

uses
  IOUtils,
  Math,

  ACBr_fpdf,
  ACBr_fpdf_ext,
  ACBr_fpdf_report,

  ACBrNFSeXClass,
  ACBrNFSeXConversao,
  ACBrDFeUtil,
  pcnConversao,

  ACBrNFSeXDANFSeFPDFClass;

const
  cCuritiba = 4106902;

{$R *.dfm}

procedure TFNFSe.Button1Click(Sender: TObject);
var
  NomeCidade: string;
  I, C: integer;
  NFSe :TNFSe;

  danfse1: TACBrNFSeXDANFSeFPDF;
begin
  ConfiguraProvedorECarregaXMLExemplo;

  if ACBrIBGE1.BuscarPorCodigo(ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio) > 0 then
    NomeCidade := ACBrIBGE1.Cidades[0].Municipio
  else
    NomeCidade := '';

  NFSe := ACBrNFSeX1.NotasFiscais[0].NFSe;

  if ckVariosItens.Checked then
    for I := 1 to 30 do
    begin
      var Item := NFSe.Servico.ItemServico.New;

      Item.Descricao := 'Serviço';
      for C := 1 to I do
        Item.Descricao := Item.Descricao + ' serviço';

      Item.Descricao := Format('%0:d - %1:s %0:d', [I, Item.Descricao]);
      Item.Quantidade := I;
      Item.ValorUnitario := I;
      Item.ValorTotal := Item.Quantidade * Item.ValorUnitario;
    end;

  if ckOutrasInformacoes.Checked then
    NFSe.OutrasInformacoes :=
      '- Esta NFS-e foi emitida com respaldo na Lei n° 14.097/2005;' +
      '- Documento emitido por ME ou EPP optante pelo Simples Nacional;' +
      '- Esta NFS-e substitui o RPS N° 4222 Série 1, emitido em 26/07/2023;';

//  if construcao then
//    NFSe.ConstrucaoCivil.CodigoObra := '1234';
//    NFSe.ConstrucaoCivil.Art := 'ART987';


  danfse1:= TACBrNFSeXDANFSeFPDF.Create(ACBrNFSeX1);
  try
    ACBrNFSeX1.DANFSE := danfse1;

      //Report.QRCode      := ckQRCode.Checked;
//    if Report.QRCode and (UmaNFSe.Link <> '') then
//        UmaNFSe.Link := 'http://www.issdigitalthe.com.br/nfse/notaFiscal.php?id_nota_fiscal=MTA2ODk2MjQy&confirma=Tg==&temPrestador=Tg==';

    if ckLogomarcaPrefeitura.Checked then
      danfse1.Logo := '..\..\logo_nfse.png'
    else
      danfse1.Logo := '';

    if ckLogomarcaPrestador.Checked then
      danfse1.Prestador.Logo := '..\..\logo_prest.png'
    else
      danfse1.Prestador.Logo := '';

    danfse1.Prefeitura := 'PREFEITURA DO MUNICIPIO DE ' + UpperCase(NomeCidade);

//    danfse1.OutrasInformacaoesImp := 'PREFEITURA DO MUNICIPIO DE CERQUILHO SECRETARIA MUNICIPAL DE FINANÇAS 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789'+
//                          'NOTA FISCAL ELETRÔNICA DE SERVIÇOS - NFe 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789';

    ACBrNFSeX1.NotasFiscais.ImprimirPDF;

  finally
    danfse1.Free;
  end;
end;

procedure TFNFSe.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

function TFNFSe.RoundTo5(Valor: Double; Casas: Integer): Double;
var
  xValor, xDecimais: String;
  p, nCasas: Integer;
  nValor: Double;
  OldRM: TFPURoundingMode;
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

    OldRM := GetRoundMode;
    try
      if Length(xDecimais) > nCasas then
      begin
        if xDecimais[nCasas + 1] >= '5' then
          SetRoundMode(rmUP)
        else
          SetRoundMode(rmNearest);
      end;

      nValor := RoundTo(Valor, Casas);

    finally
      SetRoundMode(OldRM);
    end;
  end;

  Result := nValor;
end;

procedure TFNFSe.AlimentarNFSe(NumDFe, NumLote: String);
var
  vValorISS: Double;
  i, CodigoIBGE: Integer;
  xMunicipio, xUF: string;
begin
  with ACBrNFSeX1 do
  begin
    NotasFiscais.NumeroLote := NumLote;
    NotasFiscais.Transacao := True;

    with NotasFiscais.New.NFSe do
    begin
      // Provedor CTAConsult
      TipoRecolhimento := '1';

      // Provedor PadraoNacional
      verAplic := 'ACBrNFSeX-1.00';

      // Provedor SigISS
      {
        Situação pode ser:
        tp – Tributada no prestador = tsTributadaNoPrestador;
        tt – Tributada no tomador = tsTibutadaNoTomador;
        is – Isenta = tsIsenta;
        im – Imune = tsImune;
        nt – Não tributada = tsNaoTributada.
      }
      SituacaoTrib := tsTributadaNoPrestador;

      // Usado pelo provedor AssessorPublico
      {
        A tag SITUACAO refere-se ao código da situação da NFS-e e aceita números
        inteiros de até 4 caracteres, sendo que devem estar previamente
        cadastradas no sistema.
      }
      Situacao := 1;

//      refNF := '123456789012345678901234567890123456789';
      Numero := NumDFe;
      // Provedor Infisc - Layout Proprio
      cNFSe := GerarCodigoDFe(StrToIntDef(Numero, 0));
      ModeloNFSe := '90';

      // no Caso dos provedores abaixo o campo SeriePrestacao devemos informar:
      {
        Número do equipamento emissor do RPS ou série de prestação.
        Caso não utilize a série, preencha o campo com o valor ‘99’ que indica
        modelo único. Caso queira utilizar o campo série para indicar o número do
        equipamento emissor do RPS deve-se solicitar liberação da prefeitura.
      }
      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
        SeriePrestacao := '99'
      else
        SeriePrestacao := '1';

      NumeroLote := NumLote;

      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumDFe));

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proNFSeBrasil,
        proEquiplano:
          IdentificacaoRps.Serie := '1';

        proSudoeste:
          IdentificacaoRps.Serie := 'E';

        proBetha,
        proISSDSF,
        proSiat:
          IdentificacaoRps.Serie := 'NF';

        proISSNet:
          if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
            IdentificacaoRps.Serie := '1'
          else
            IdentificacaoRps.Serie := '8';

        proSystemPro:
          IdentificacaoRps.Serie := 'RPP';

        proPadraoNacional:
          IdentificacaoRps.Serie := '900';
      else
        IdentificacaoRps.Serie := '85';
      end;

      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      IdentificacaoRps.Tipo := trRPS;

      DataEmissao := Now;
      Competencia := Now;
      DataEmissaoRPS := Now;
      // Provedor RLZ
      Vencimento := Now + 1;
      // Provedor fintelISS
      DataPagamento := Now;

      (*
        TnfseNaturezaOperacao = ( no1, no2, no3, no4, no5, no6, no7,
        no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
        no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
        no70, no71, no72, no78, no79,
        no101, no111, no121, no201, no301,
        no501, no511, no541, no551, no601, no701 );
      *)

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        // Provedor Thema:
        // 50|51|52|53|54|55|56|57|58|59|60|61|62|63|64|65|66|67|68|69|70|71|72|78|79
        proThema:
          NaturezaOperacao := no51;
      else
        NaturezaOperacao := no1;
      end;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proPadraoNacional:
          RegimeEspecialTributacao := retCooperativa;
      else
        RegimeEspecialTributacao := retMicroempresaMunicipal;
      end;

      // TnfseSimNao = ( snSim, snNao );
      OptanteSimplesNacional := snSim;

      // Provedor PadraoNacional (osnNaoOptante, osnOptanteMEI, osnOptanteMEEPP)
      OptanteSN := osnOptanteMEI;

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
      if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
        Producao := snSim
      else
        Producao := snNao;

      // TnfseStatusRPS = ( srNormal, srCancelado );
      StatusRps := srNormal;

      // Somente Os provedores Betha, FISSLex e SimplISS permitem incluir no RPS
      // a TAG: OutrasInformacoes os demais essa TAG é gerada e preenchida pelo
      // WebService do provedor.
      OutrasInformacoes := 'Pagamento a Vista';

      {=========================================================================
        Numero, Série e Tipo do Rps que esta sendo substituido por este
      =========================================================================}

      {
       RpsSubstituido.Numero := FormatFloat('#########0', i);
       RpsSubstituido.Serie  := 'UNICA';
       // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
       RpsSubstituido.Tipo   := trRPS;
      }

      {=========================================================================
        Dados do Serviço (valores)
      =========================================================================}

      // Provedores que permitem informar mais de 1 serviço:
      if (ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAgili, proAssessorPublico,
           proCTA, proCTAConsult, proEloTech, proEquiplano, proFacundo, proFGMaiss,
           profintelISS, proGoverna, proInfisc, proIPM, proISSDSF, proPriMax,
           proRLZ, proSimple, proSmarAPD, proWebFisco, proBauhaus,
           proSoftPlan]) or
         ((ACBrNFSeX1.Configuracoes.Geral.Provedor in [proEL, proSimplISS]) and
          (ACBrNFSeX1.Configuracoes.Geral.Versao = ve100)) then
      begin
        // Provedor Elotech
        Servico.Valores.RetidoPis := snNao;
        Servico.Valores.RetidoCofins := snNao;
        Servico.Valores.AliquotaInss := 0;
        Servico.Valores.RetidoInss := snNao;
        Servico.Valores.AliquotaIr := 0;
        Servico.Valores.RetidoIr := snNao;
        Servico.Valores.AliquotaCsll := 0;
        Servico.Valores.RetidoCsll := snNao;

        with Servico.ItemServico.New do
        begin
          Descricao := 'Desc. do Serv. 1';
          ItemListaServico := '09.01';

          // infisc, EL
          CodServ := '12345';
          // Infisc, EL
          codLCServ := '123';

          ValorDeducoes := 0;
          xJustDeducao := '';

          AliqReducao := 0;
          ValorReducao := 0;

          DescontoIncondicionado := 0;
          DescontoCondicionado := 0;

          // TUnidade = (tuHora, tuQtde);
          TipoUnidade := tuQtde;
          Unidade := 'UN';
          Quantidade := 10;
          ValorUnitario := 0.01;

          QtdeDiaria := 0;
          ValorTaxaTurismo := 0;

          ValorTotal := Quantidade * ValorUnitario;

          BaseCalculo := ValorTotal - ValorDeducoes - DescontoIncondicionado;

          Aliquota := 0.10;

          ValorISS := BaseCalculo * Aliquota / 100;

          ValorISSRetido := 0;

          AliqISSST := 0;
          ValorISSST := 0;

          ValorBCCSLL := 0;
          AliqRetCSLL := 0;
          ValorCSLL := 0;

          ValorBCPIS := 0;
          AliqRetPIS := 0;
          ValorPIS := 0;

          ValorBCCOFINS := 0;
          AliqRetCOFINS := 0;
          ValorCOFINS := 0;

          ValorBCINSS := 0;
          AliqRetINSS := 0;
          ValorINSS := 0;

          ValorBCRetIRRF := 0;
          AliqRetIRRF := 0;
          ValorIRRF := 0;

          // Provedor EloTech
          Tributavel := snNao;

          case ACBrNFSeX1.Configuracoes.Geral.Provedor of
            proAgili:
              // código com 9 digitos
              CodigoCnae := '452000200';
          else
            // código com 7 digitos
            CodigoCnae := '6203100';
          end;

          // Provedor IPM
          { define se o tributo é no municipio do prestador ou não }
          TribMunPrestador := snNao;
          { codigo do municipio que ocorreu a prestação de serviço }
          CodMunPrestacao := '4106902';
          { codigo da situação tributária: 0 até 15 }
          SituacaoTributaria := 0;
        end;
      end
      else
      begin
        Servico.Valores.ValorServicos := 100.35;
        Servico.Valores.ValorDeducoes := 0.00;
        Servico.Valores.AliquotaPis := 0.00;
        Servico.Valores.ValorPis := 0.00;
        Servico.Valores.AliquotaCofins := 2.00;
        Servico.Valores.ValorCofins := 2.00;
        Servico.Valores.ValorInss := 0.00;
        Servico.Valores.ValorIr := 0.00;
        Servico.Valores.ValorCsll := 0.00;
        // Usado pelo provedor SystemPro
        Servico.Valores.ValorTaxaTurismo := 0.00;
        Servico.Valores.QtdeDiaria := 0.00;

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

        Servico.Valores.Aliquota := 2;

        // Provedor PadraoNacional
        Servico.Valores.tribMun.tribISSQN := tiOperacaoTributavel;
        Servico.Valores.tribMun.cPaisResult := 0;

        Servico.Valores.tribFed.CST := cst01;
        Servico.Valores.tribFed.vBCPisCofins := Servico.Valores.BaseCalculo;
        Servico.Valores.tribFed.pAliqPis := 1.65;
        Servico.Valores.tribFed.pAliqCofins := 7.60;
        Servico.Valores.tribFed.vPis := Servico.Valores.tribFed.vBCPisCofins *
                                         Servico.Valores.tribFed.pAliqPis / 100;
        Servico.Valores.tribFed.vCofins := Servico.Valores.tribFed.vBCPisCofins *
                                      Servico.Valores.tribFed.pAliqCofins / 100;
        Servico.Valores.tribFed.tpRetPisCofins := trpcNaoRetido;

        Servico.Valores.totTrib.vTotTribFed := Servico.Valores.tribFed.vPis;
        Servico.Valores.totTrib.vTotTribEst := 0;
        Servico.Valores.totTrib.vTotTribMun := Servico.Valores.tribFed.vCofins;


        vValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota / 100;

        // A função RoundTo5 é usada para arredondar valores, sendo que o segundo
        // parametro se refere ao numero de casas decimais.
        // exemplos: RoundTo5(50.532, -2) ==> 50.53
        // exemplos: RoundTo5(50.535, -2) ==> 50.54
        // exemplos: RoundTo5(50.536, -2) ==> 50.54

        Servico.Valores.ValorISS := RoundTo5(vValorISS, -2);

        Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
          Servico.Valores.ValorPis - Servico.Valores.ValorCofins -
          Servico.Valores.ValorInss - Servico.Valores.ValorIr -
          Servico.Valores.ValorCsll - Servico.Valores.OutrasRetencoes -
          Servico.Valores.ValorIssRetido - Servico.Valores.DescontoIncondicionado
          - Servico.Valores.DescontoCondicionado;
      end;

      {=========================================================================
        Dados do Serviço
      =========================================================================}

      if ACBrNFSeX1.Configuracoes.Geral.Provedor = proInfisc then
      begin
        Servico.Valores.ValorServicos := 100.35;
        Servico.Valores.DescontoIncondicionado := 0.00;
        Servico.Valores.OutrosDescontos := 0.00;

        Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
          Servico.Valores.ValorPis - Servico.Valores.ValorCofins -
          Servico.Valores.ValorInss - Servico.Valores.ValorIr -
          Servico.Valores.ValorCsll - Servico.Valores.OutrasRetencoes -
          Servico.Valores.ValorIssRetido - Servico.Valores.DescontoIncondicionado
          - Servico.Valores.DescontoCondicionado;
      end;

      DeducaoMateriais := snSim;
      with Servico.Deducao.New do
      begin
        DeducaoPor := dpValor;
        TipoDeducao := tdMateriais;
        ValorDeduzir := 10.00;
      end;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSiapSistemas:
          // código padrão ABRASF acrescido de um sub-item
          Servico.ItemListaServico := '01.05.00';

        proPadraoNacional:
          Servico.ItemListaServico := '010201';

        proCTAConsult:
          Servico.ItemListaServico := '01050';
      else
        // código padrão da ABRASF
        Servico.ItemListaServico := '09.01';
      end;

      servico.CodigoNBS := '123456789';
      Servico.Discriminacao := 'discriminacao I; discriminacao II';

      // TnfseResponsavelRetencao = ( rtTomador, rtPrestador, rtIntermediario, rtNenhum )
      //                              '1',       '',          '2',             ''
      Servico.ResponsavelRetencao := rtTomador;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proISSSJP:
          Servico.CodigoTributacaoMunicipio := '631940000';

        proCenti:
          Servico.CodigoTributacaoMunicipio := '0901';

        proISSSalvador:
          Servico.CodigoTributacaoMunicipio := '0901001';

        proIPM, proSystemPro:
          Servico.CodigoTributacaoMunicipio := '';

        proPadraoNacional:
          Servico.CodigoTributacaoMunicipio := '';
      else
        Servico.CodigoTributacaoMunicipio := '63194';
      end;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSiat, proISSDSF, proCTAConsult:
          // código com 9 digitos
          Servico.CodigoCnae := '452000200';

        proSystemPro:
          Servico.CodigoCnae := '';
      else
        // código com 7 digitos
        Servico.CodigoCnae := '6203100';
      end;

      // Para o provedor ISSNet em ambiente de Homologação
      // o Codigo do Municipio tem que ser '999'
      Servico.CodigoMunicipio := cCuritiba.ToString;
      Servico.UFPrestacao := 'PR';

      // Informar A Exigibilidade ISS para fintelISS [1/2/3/4/5/6/7]
      Servico.ExigibilidadeISS := exiExigivel;

      Servico.CodigoPais := 1058; // Brasil
      Servico.MunicipioIncidencia := cCuritiba;

      // Provedor GeisWeb
      // tlDevidoNoMunicPrestador, tlDevidoNoMunicTomador, tlSimplesNacional, tlIsentoImune
      Servico.TipoLancamento := tlSimplesNacional;

      {=========================================================================
        Dados do Prestador de Serviço
      =========================================================================}
      Prestador.IdentificacaoPrestador.CpfCnpj := '46363985000110';
      Prestador.IdentificacaoPrestador.InscricaoMunicipal := '010610196855';
      Prestador.RazaoSocial := 'WA2 TECNOLOGIA LTDA';
      Prestador.NomeFantasia := 'NUVEM FISCAL';
      // Para o provedor ISSDigital deve-se informar também:
      Prestador.cUF := UFtoCUF('PR');

      Prestador.Endereco.Endereco := 'R HEITOR STOCKLER DE FRANÇA';
      Prestador.Endereco.Numero := '396';
      Prestador.Endereco.Bairro := 'CENTRO CÍVICO';
      Prestador.Endereco.CodigoMunicipio := cCuritiba.ToString;

      CodigoIBGE := cCuritiba;

      if CodigoIBGE > 0 then
        xMunicipio := ObterNomeMunicipio(CodigoIBGE, xUF);

      Prestador.Endereco.xMunicipio := xMunicipio;
      Prestador.Endereco.UF := 'PR';
      Prestador.Endereco.CodigoPais := 1058;
      Prestador.Endereco.xPais := 'BRASIL';
      Prestador.Endereco.CEP := '80030030';

      Prestador.Contato.DDD := '16';

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSigep, proCTAConsult:
          Prestador.Contato.Telefone := '33224455';
      else
        Prestador.Contato.Telefone := '1633224455';
      end;

      Prestador.Contato.Email := 'prestador@provedor.com.br';

      {=========================================================================
        Dados do Tomador de Serviço
      =========================================================================}

      Tomador.AtualizaTomador := snNao;
      Tomador.TomadorExterior := snNao;

      // Para o provedor IPM usar os valores:
      // tpPFNaoIdentificada ou tpPF para pessoa Fisica
      // tpPJdoMunicipio ou tpPJforaMunicipio ou tpPJforaPais para pessoa Juridica

      // Para o provedor SigISS usar os valores acima de forma adquada
      Tomador.IdentificacaoTomador.Tipo := tpPF;
      Tomador.IdentificacaoTomador.CpfCnpj := '12345678901';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '123456-7';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '654321-0';

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE E TESTE';

      // O campo EnderecoInformado é utilizado pelo provedor IPM
      // A tag <endereco_informado> é opcional, caso não deseje que ela seja
      // gerada devemos informar uma string vazia, ou S = Sim ou N = Não
      Tomador.Endereco.EnderecoInformado := 'S';
      Tomador.Endereco.TipoLogradouro := 'RUA';
      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.TipoBairro := 'BAIRRO';
      Tomador.Endereco.Bairro := 'CENTRO';
      Tomador.Endereco.CodigoMunicipio := cCuritiba.ToString;

      CodigoIBGE := cCuritiba;

      if CodigoIBGE > 0 then
        xMunicipio := ObterNomeMunicipio(CodigoIBGE, xUF);

      Tomador.Endereco.xMunicipio := xMunicipio;
      Tomador.Endereco.UF := 'PR';
      Tomador.Endereco.CodigoPais := 1058; // Brasil
      Tomador.Endereco.CEP := '14800000';
      Tomador.Endereco.xPais := 'BRASIL';

      Tomador.Contato.DDD := '16';

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSigep, proCTAConsult:
          Tomador.Contato.Telefone := '22223333';
      else
        Tomador.Contato.Telefone := '1622223333';
      end;

      Tomador.Contato.Email := 'tomador@provedor.com.br';

      {=========================================================================
        Dados do Intermediario na prestação do serviço
      =========================================================================}

//       Intermediario.RazaoSocial := 'razao';
//       Intermediario.Identificacao.CpfCnpj := '00000000000';
//       Intermediario.Identificacao.InscricaoMunicipal := '12547478';

      {=========================================================================
        Dados da Obra (quando o serviço for uma obra)
      =========================================================================}

//      ConstrucaoCivil.CodigoObra := '88888';
//      ConstrucaoCivil.Art        := '433';

      {=========================================================================
        Dados da Condição de Pagamento (no momento somente o provedor Betha
        versão 1 do Layout da ABRASF)
      =========================================================================}

      if ACBrNFSeX1.Configuracoes.Geral.Provedor = proBetha then
      begin
        // Condicao = (cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoCredito, cpCartaoDebito)
        CondicaoPagamento.Condicao   := cpAVista;
        CondicaoPagamento.QtdParcela := 1;

        for i := 1 to CondicaoPagamento.QtdParcela do
        begin
          with CondicaoPagamento.Parcelas.New do
          begin
            Parcela := IntToStr(i);
            DataVencimento := Date + (30 * i);
            Valor := (Servico.Valores.ValorLiquidoNfse / CondicaoPagamento.QtdParcela);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFNFSe.ConfiguraProvedorECarregaXMLExemplo;
var
  Xml: string;
begin
  case rbProvedor.ItemIndex of
    0: // ISS Curitiba
      begin
        Xml := TFile.ReadAllText('..\..\nfse-isscuritiba.xml');
        ACBrNFSeX1.Configuracoes.Geral.LayoutNFSe := lnfsProvedor;
        ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 4106902;
      end;

    1: // ISSDSF
      begin
        Xml := TFile.ReadAllText('..\..\nfse-issdsf.xml');
        ACBrNFSeX1.Configuracoes.Geral.LayoutNFSe := lnfsProvedor;
        ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 2211001;
      end;

    2: // ISS São Paulo
      begin
        Xml := TFile.ReadAllText('..\..\nfse-isssp.xml');
        ACBrNFSeX1.Configuracoes.Geral.LayoutNFSe := lnfsProvedor;
        ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 3550308;
      end;

    3: // Betha
      begin
        Xml := TFile.ReadAllText('..\..\nfse-betha.xml');
        ACBrNFSeX1.Configuracoes.Geral.LayoutNFSe := lnfsProvedor;
        ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 4215455;
      end;

    4: // Ginfes
      begin
        Xml := TFile.ReadAllText('..\..\nfse-ginfes.xml');
        ACBrNFSeX1.Configuracoes.Geral.LayoutNFSe := lnfsProvedor;
        ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 3547809;
      end;

    5: // Tiplan
      begin
        Xml := TFile.ReadAllText('..\..\nfse-tiplan.xml');
        ACBrNFSeX1.Configuracoes.Geral.LayoutNFSe := lnfsProvedor;
        ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 3304524;
      end;
  else
    raise Exception.Create('Provedor não reconhecido');
  end;

  ACBrNFSeX1.NotasFiscais.Clear;
  ACBrNFSeX1.NotasFiscais.LoadFromString(Xml);
end;

end.
