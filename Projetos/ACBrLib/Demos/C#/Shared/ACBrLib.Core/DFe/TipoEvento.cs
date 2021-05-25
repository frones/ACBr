using ACBrLib.Core.Ini;

namespace ACBrLib.Core.DFe
{
    public enum TipoEvento
    {
        [EnumValue("-99999")]
        teNaoMapeado,

        [EnumValue("110110")]
        teCCe,

        [EnumValue("110111")]
        teCancelamento,

        [EnumValue("210200")]
        teManifDestConfirmacao,

        [EnumValue("210210")]
        teManifDestCiencia,

        [EnumValue("210220")]
        teManifDestDesconhecimento,

        [EnumValue("210240")]
        teManifDestOperNaoRealizada,

        [EnumValue("110112")]
        teEncerramento,

        [EnumValue("110113")]
        teEPEC,

        [EnumValue("110114")]
        teInclusaoCondutor,

        [EnumValue("110160")]
        teMultiModal,

        [EnumValue("310620")]
        teRegistroPassagem,

        [EnumValue("510620")]
        teRegistroPassagemBRId,

        [EnumValue("110140")]
        teEPECNFe,

        [EnumValue("610600")]
        teRegistroCTe,

        [EnumValue("610501")]
        teRegistroPassagemNFeCancelado,

        [EnumValue("610550")]
        teRegistroPassagemNFeRFID,

        [EnumValue("610601")]
        teCTeCancelado,

        [EnumValue("610611")]
        teMDFeCancelado,

        [EnumValue("990900")]
        teVistoriaSuframa,

        [EnumValue("111500")]
        tePedProrrog1,

        [EnumValue("111501")]
        tePedProrrog2,

        [EnumValue("111502")]
        teCanPedProrrog1,

        [EnumValue("111503")]
        teCanPedProrrog2,

        [EnumValue("411500")]
        teEventoFiscoPP1,

        [EnumValue("411501")]
        teEventoFiscoPP2,

        [EnumValue("411502")]
        teEventoFiscoCPP1,

        [EnumValue("411503")]
        teEventoFiscoCPP2,

        [EnumValue("610500")]
        teRegistroPassagemNFe,

        [EnumValue("990910")]
        teConfInternalizacao,

        [EnumValue("000000")]
        teCTeAutorizado,

        [EnumValue("610610")]
        teMDFeAutorizado,

        [EnumValue("610110")]
        tePrestDesacordo,

        [EnumValue("110170")]
        teGTV,

        [EnumValue("310610")]
        teMDFeAutorizado2,

        [EnumValue("110115")]
        teNaoEmbarque,

        [EnumValue("310611")]
        teMDFeCancelado2,

        [EnumValue("610614")]
        teMDFeAutorizadoComCTe,

        [EnumValue("610510")]
        teRegPasNfeProMDFe,

        [EnumValue("610514")]
        teRegPasNfeProMDFeCte,

        [EnumValue("610554")]
        teRegPasAutMDFeComCte,

        [EnumValue("610615")]
        teCancelamentoMDFeAutComCTe,

        [EnumValue("790700")]
        teAverbacaoExportacao,

        [EnumValue("240130")]
        teAutCteComplementar,

        [EnumValue("240131")]
        teCancCteComplementar,

        [EnumValue("240140")]
        teCTeSubstituicao,

        [EnumValue("240150")]
        teCTeAnulacao,

        [EnumValue("240160")]
        teLiberacaoEPEC,

        [EnumValue("240170")]
        teLiberacaoPrazoCanc,

        [EnumValue("440130")]
        teAutorizadoRedespacho,

        [EnumValue("440140")]
        teautorizadoRedespIntermed,

        [EnumValue("440150")]
        teAutorizadoSubcontratacao,

        [EnumValue("440160")]
        teautorizadoServMultimodal,

        [EnumValue("110112")]
        teCancSubst,

        [EnumValue("110116")]
        teAlteracaoPoltrona,

        [EnumValue("110180")]
        teComprEntrega,

        [EnumValue("110181")]
        teCancComprEntrega,

        [EnumValue("110115")]
        teInclusaoDFe,

        [EnumValue("240140")]
        teAutorizadoSubstituicao,

        [EnumValue("240150")]
        teAutorizadoAjuste,

        [EnumValue("240170")]
        teLiberacaoPrazoCancelado,

        [EnumValue("110116")]
        tePagamentoOperacao,

        [EnumValue("110117")]
        teExcessoBagagem,

        [EnumValue("310112")]
        teEncerramentoFisco,

        [EnumValue("110130")]
        teComprEntregaNFe,

        [EnumValue("110131")]
        teCancComprEntregaNFe,

        [EnumValue("110150")]
        teAtorInteressadoNFe,

        [EnumValue("610130")]
        teComprEntregaCTe,

        [EnumValue("610131")]
        teCancComprEntregaCTe
    }
}