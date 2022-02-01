using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public enum TipoEventoNFe
    {
        [EnumValue("-99999")]
        teNaoMapeado,

        [EnumValue("110110")]
        teCCe,

        [EnumValue("110111")]
        teCancelamento,

        [EnumValue("110112")]
        teCancSubst,
		
		[EnumValue("110113")]
        teEPEC,

        [EnumValue("110140")]
        teEPECNFe,

        [EnumValue("111500")]
        tePedProrrog1,

        [EnumValue("111501")]
        tePedProrrog2,

        [EnumValue("111502")]
        teCanPedProrrog1,

        [EnumValue("111503")]
        teCanPedProrrog2,

        [EnumValue("210200")]
        teManifDestConfirmacao,

        [EnumValue("210210")]
        teManifDestCiencia,

        [EnumValue("210220")]
        teManifDestDesconhecimento,

        [EnumValue("210240")]
        teManifDestOperNaoRealizada,

        [EnumValue("610600")]
        teRegistroCTe,

        [EnumValue("610614")]
        teMDFeAutorizadoComCTe,

        [EnumValue("790700")]
        teAverbacaoExportacao,

        [EnumValue("990900")]
        teVistoriaSuframa,

        [EnumValue("990910")]
        teConfInternalizacao,

        [EnumValue("110180")]
        teComprEntrega,

        [EnumValue("110181")]
        teCancComprEntrega,

        [EnumValue("610554")]
        teRegPasAutMDFeComCte,

        [EnumValue("610510")]
        teRegPasNfeProMDFe,

        [EnumValue("610615")]
        teCancelamentoMDFeAutComCTe,

        [EnumValue("610610")]
        teMDFeAutorizado,

        [EnumValue("110130")]
        teComprEntregaNFe,

        [EnumValue("110131")]
        teCancComprEntregaNFe,

        [EnumValue("110150")]
        teAtorInteressadoNFe,

        [EnumValue("610130")]
        teComprEntregaCTe,

        [EnumValue("610131")]
        teCancComprEntregaCTe,

        [EnumValue("610601")]
        teCTeCancelado,
    }
}