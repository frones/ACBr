using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public enum IndicadorPagamento
    {
        [EnumValue("0")] ipVista,

        [EnumValue("1")] ipPrazo,

        [EnumValue("2")] ipOutras,

        [EnumValue("")] ipNenhum
    }

    public enum DestinoOperacao
    {
        doInterna = 1,
        doInterestadual = 2,
        doExterior = 3
    }

    public enum FinalidadeNFe
    {
        fnNormal = 1,
        fnComplementar = 2,
        fnAjuste = 3,
        fnDevolucao = 4
    }

    public enum ConsumidorFinal
    {
        cfNao = 0,
        cfConsumidorFina = 1
    }

    public enum PresencaComprador
    {
        pcNao = 0,
        pcPresencial = 1,
        pcInternet = 2,
        pcTeleatendimento = 3,
        pcEntregaDomicilio = 4,
        pcPresencialForaEstabelecimento = 5,
        pcOutros = 9
    }

    public enum ProcessoEmissao
    {
        peAplicativoContribuinte = 0,
        peAvulsaFisco = 1,
        peAvulsaContribuinte = 2,
        peContribuinteAplicativoFisco = 3
    }

    public enum IndIntermed
    {
        [EnumValue("")] iiSemOperacao,

        [EnumValue("0")] iiOperacaoSemIntermediador,

        [EnumValue("1")] iiOperacaoComIntermediador
    }

    public enum TipoRef
    {
        [EnumValue("NF")] NF,

        [EnumValue("NFe")] NFe,

        [EnumValue("NFP")] NFP,

        [EnumValue("CTe")] CTe,

        [EnumValue("ECF")] ECF
    }

    public enum IndicadorTotal
    {
        itNaoSomaTotalNFe = 0,
        itSomaTotalNFe = 1
    }

    public enum IndEscala
    {
        [EnumValue("")] ieNenhum,

        [EnumValue("S")] ieRelevante,

        [EnumValue("N")] ieNaoRelevante
    }

    public enum TipoViaTransp
    {
        tvMaritima = 1,
        tvFluvial = 2,
        tvLacustre = 3,
        tvAerea = 4,
        tvPostal = 5,
        tvFerroviaria = 6,
        tvRodoviaria = 7,
        tvConduto = 8,
        tvMeiosProprios = 9,
        tvEntradaSaidaFicta = 10,
        tvCourier = 11,
        tvHandcarry = 12
    }

    public enum TipoArma
    {
        taUsoPermitido = 0,
        taUsoRestrito = 1
    }

    public enum TipoOperacao
    {
        toOutros = 0,
        toVendaConcessionaria = 1,
        toFaturamentoDireto = 2,
        toVendaDireta = 3
    }

    public enum CondicaoVeiculo
    {
        cvAcabado = 1,
        cvInacabado = 2,
        cvSemiAcabado = 3
    }

    public enum OrigemMercadoria
    {
        oeNacional = 0,
        oeEstrangeiraImportacaoDireta = 1,
        oeEstrangeiraAdquiridaBrasil = 2,
        oeNacionalConteudoImportacaoSuperior40 = 3,
        oeNacionalProcessosBasicos = 4,
        oeNacionalConteudoImportacaoInferiorIgual40 = 5,
        oeEstrangeiraImportacaoDiretaSemSimilar = 6,
        oeEstrangeiraAdquiridaBrasilSemSimilar = 7,
        oeNacionalConteudoImportacaoSuperior70 = 8
    }

    public enum CSTIcms
    {
        [EnumValue("")] cstVazio,

        [EnumValue("00")] cst00,

        [EnumValue("10")] cst10,

        [EnumValue("20")] cst20,

        [EnumValue("30")] cst30,

        [EnumValue("40")] cst40,

        [EnumValue("41")] cst41,

        [EnumValue("45")] cst45,

        [EnumValue("50")] cst50,

        [EnumValue("51")] cst51,

        [EnumValue("60")] cst60,

        [EnumValue("70")] cst70,

        [EnumValue("80")] cst80,

        [EnumValue("81")] cst81,

        [EnumValue("90")] cst90,

        [EnumValue("91")] cstICMSOutraUF,

        [EnumValue("SN")] cstICMSSN,

        [EnumValue("10part")] cstPart10,

        [EnumValue("90part")] cstPart90,

        [EnumValue("41rep")] cstRep41,

        [EnumValue("60rep")] cstRep60
    }

    public enum CSOSNIcms
    {
        [EnumValue("")] csosnVazio,

        [EnumValue("101")] csosn101,

        [EnumValue("102")] csosn102,

        [EnumValue("103")] csosn103,

        [EnumValue("201")] csosn201,

        [EnumValue("202")] csosn202,

        [EnumValue("203")] csosn203,

        [EnumValue("300")] csosn300,

        [EnumValue("400")] csosn400,

        [EnumValue("500")] csosn500,

        [EnumValue("900")] csosn900
    }

    public enum DeterminacaoBaseIcms
    {
        [EnumValue("")] dbiNenhum,

        [EnumValue("0")] dbiMargemValorAgregado,

        [EnumValue("1")] dbiPauta,

        [EnumValue("2")] dbiPrecoTabelado,

        [EnumValue("3")] dbiValorOperacao
    }

    public enum DeterminacaoBaseIcmsST
    {
        dbisPrecoTabelado = 0,
        dbisListaNegativa = 1,
        dbisListaPositiva = 2,
        dbisListaNeutra = 3,
        dbisMargemValorAgregado = 4,
        dbisPauta = 5,
        dbisValordaOperacao = 6
    }

    public enum MotivoDesoneracaoICMS
    {
        mdiTaxi = 1,
        mdiDeficienteFisico = 2,
        mdiProdutorAgropecuario = 3,
        mdiFrotistaLocadora = 4,
        mdiDiplomaticoConsular = 5,
        mdiAmazoniaLivreComercio = 6,
        mdiSuframa = 7,
        mdiVendaOrgaosPublicos = 8,
        mdiOutros = 9,
        mdiDeficienteCondutor = 10,
        mdiDeficienteNaoCondutor = 11,
        mdiOrgaoFomento = 12,
        mdiOlimpiadaRio2016 = 16,
        mdiSolicitadoFisco = 90
    }

    public enum CSTIPI
    {
        [EnumValue("00")]
        ipi00,

        [EnumValue("49")]
        ipi49,

        [EnumValue("50")]
        ipi50,

        [EnumValue("99")]
        ipi99,

        [EnumValue("01")]
        ipi01,

        [EnumValue("02")]
        ipi02,

        [EnumValue("03")]
        ipi03,

        [EnumValue("04")]
        ipi04,

        [EnumValue("05")]
        ipi05,

        [EnumValue("51")]
        ipi51,

        [EnumValue("52")]
        ipi52,

        [EnumValue("53")]
        ipi53,

        [EnumValue("54")]
        ipi54,

        [EnumValue("55")]
        ipi55
    }

    public enum CSTPIS
    {
        [EnumValue("01")]
        pis01,

        [EnumValue("02")]
        pis02,

        [EnumValue("03")]
        pis03,

        [EnumValue("04")]
        pis04,

        [EnumValue("05")]
        pis05,

        [EnumValue("06")]
        pis06,

        [EnumValue("07")]
        pis07,

        [EnumValue("08")]
        pis08,

        [EnumValue("09")]
        pis09,

        [EnumValue("49")]
        pis49,

        [EnumValue("50")]
        pis50,

        [EnumValue("51")]
        pis51,

        [EnumValue("52")]
        pis52,

        [EnumValue("53")]
        pis53,

        [EnumValue("54")]
        pis54,

        [EnumValue("55")]
        pis55,

        [EnumValue("56")]
        pis56,

        [EnumValue("60")]
        pis60,

        [EnumValue("61")]
        pis61,

        [EnumValue("62")]
        pis62,

        [EnumValue("63")]
        pis63,

        [EnumValue("64")]
        pis64,

        [EnumValue("65")]
        pis65,

        [EnumValue("66")]
        pis66,

        [EnumValue("67")]
        pis67,

        [EnumValue("70")]
        pis70,

        [EnumValue("71")]
        pis71,

        [EnumValue("72")]
        pis72,

        [EnumValue("73")]
        pis73,

        [EnumValue("74")]
        pis74,

        [EnumValue("75")]
        pis75,

        [EnumValue("98")]
        pis98,

        [EnumValue("99")]
        pis99
    }

    public enum CSTCofins
    {
        [EnumValue("01")]
        cof01,

        [EnumValue("02")]
        cof02,

        [EnumValue("03")]
        cof03,

        [EnumValue("04")]
        cof04,

        [EnumValue("05")]
        cof05,

        [EnumValue("06")]
        cof06,

        [EnumValue("07")]
        cof07,

        [EnumValue("08")]
        cof08,

        [EnumValue("09")]
        cof09,

        [EnumValue("49")]
        cof49,

        [EnumValue("50")]
        cof50,

        [EnumValue("51")]
        cof51,

        [EnumValue("52")]
        cof52,

        [EnumValue("53")]
        cof53,

        [EnumValue("54")]
        cof54,

        [EnumValue("55")]
        cof55,

        [EnumValue("56")]
        cof56,

        [EnumValue("60")]
        cof60,

        [EnumValue("61")]
        cof61,

        [EnumValue("62")]
        cof62,

        [EnumValue("63")]
        cof63,

        [EnumValue("64")]
        cof64,

        [EnumValue("65")]
        cof65,

        [EnumValue("66")]
        cof66,

        [EnumValue("67")]
        cof67,

        [EnumValue("70")]
        cof70,

        [EnumValue("71")]
        cof71,

        [EnumValue("72")]
        cof72,

        [EnumValue("73")]
        cof73,

        [EnumValue("74")]
        cof74,

        [EnumValue("75")]
        cof75,

        [EnumValue("98")]
        cof98,

        [EnumValue("99")]
        cof99
    }

    public enum ISSQNcSitTrib
    {
        [EnumValue("")]
        ISSQNcSitTribVazio,

        [EnumValue("N")]
        ISSQNcSitTribNORMAL,

        [EnumValue("R")]
        ISSQNcSitTribRETIDA,

        [EnumValue("S")]
        ISSQNcSitTribSUBSTITUTA,

        [EnumValue("I")]
        ISSQNcSitTribISENTA
    }

    public enum IndISS
    {
        iiExigivel = 1,
        iiNaoIncidencia = 2,
        iiIsencao = 3,
        iiExportacao = 4,
        iiImunidade = 5,
        iiExigSuspDecisaoJudicial = 6,
        iiExigSuspProcessoAdm = 7
    }

    public enum IndIncentivo
    {
        iiSim = 1,
        iiNao = 2
    }

    public enum RegTribISSQN
    {
        RTISSNenhum = 0,
        RTISSMicroempresaMunicipal = 1,
        RTISSEstimativa = 2,
        RTISSSociedadeProfissionais = 3,
        RTISSCooperativa = 4,
        RTISSMEI = 5,
        RTISSMEEPP = 6
    }

    public enum ModalidadeFrete
    {
        mfContaEmitente = 0,
        mfContaDestinatario = 1,
        mfContaTerceiros = 2,
        mfProprioRemetente = 3,
        mfProprioDestinatario = 4,
        mfSemFrete = 9
    }

    public enum FormaPagamento
    {
        [EnumValue("01")]
        fpDinheiro,

        [EnumValue("02")]
        fpCheque,

        [EnumValue("03")]
        fpCartaoCredito,

        [EnumValue("04")]
        fpCartaoDebito,

        [EnumValue("05")]
        fpCreditoLoja,

        [EnumValue("10")]
        fpValeAlimentacao,

        [EnumValue("11")]
        fpValeRefeicao,

        [EnumValue("12")]
        fpValePresente,

        [EnumValue("13")]
        fpValeCombustivel,

        [EnumValue("14")]
        fpDuplicataMercantil,

        [EnumValue("15")]
        fpBoletoBancario,

        [EnumValue("16")]
        fpDepositoBancario,

        [EnumValue("17")]
        fpPagamentoInstantaneo,

        [EnumValue("18")]
        fpTransfBancario,

        [EnumValue("19")]
        fpProgramaFidelidade,

        [EnumValue("90")]
        fpSemPagamento,

        [EnumValue("98")]
        fpRegimeEspecial,

        [EnumValue("99")]
        fpOutro
    }

    public enum TpIntegra
    {
        [EnumValue("")]
        tiNaoInformado,

        [EnumValue("1")]
        tiPagIntegrado,

        [EnumValue("2")]
        tiPagNaoIntegrado
    }

    public enum BandeiraCartao
    {
        [EnumValue("01")]
        bcVisa,

        [EnumValue("02")]
        bcMasterCard,

        [EnumValue("03")]
        bcAmericanExpress,

        [EnumValue("04")]
        bcSorocred,

        [EnumValue("05")]
        bcDinersClub,

        [EnumValue("06")]
        bcElo,

        [EnumValue("07")]
        bcHipercard,

        [EnumValue("08")]
        bcAura,

        [EnumValue("09")]
        bcCabal,

        [EnumValue("10")]
        bcAlelo,

        [EnumValue("11")]
        bcBanesCard,

        [EnumValue("12")]
        bcCalCard,

        [EnumValue("13")]
        bcCredz,

        [EnumValue("14")]
        bcDiscover,

        [EnumValue("15")]
        bcGoodCard,

        [EnumValue("16")]
        bcGrenCard,

        [EnumValue("17")]
        bcHiper,

        [EnumValue("18")]
        bcJcB,

        [EnumValue("19")]
        bcMais,

        [EnumValue("20")]
        bcMaxVan,

        [EnumValue("21")]
        bcPolicard,

        [EnumValue("22")]
        bcRedeCompras,

        [EnumValue("23")]
        bcSodexo,

        [EnumValue("24")]
        bcValeCard,

        [EnumValue("25")]
        bcVerocheque,

        [EnumValue("26")]
        bcVR,

        [EnumValue("27")]
        bcTicket,

        [EnumValue("99")]
        bcOutros
    }

    public enum IndicadorProcesso
    {
        ipSEFAZ = 0,
        ipJusticaFederal = 1,
        ipJusticaEstadual = 2,
        ipSecexRFB = 3,
        ipOutros = 9
    }
}