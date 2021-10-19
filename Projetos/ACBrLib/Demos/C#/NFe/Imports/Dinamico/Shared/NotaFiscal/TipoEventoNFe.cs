namespace ACBrLib.Core.DFe
{
    public enum TipoEventoNFe
    {
        [EnumValue("110110")]
        teCCe,

        [EnumValue("110111")]
        teCancelamento,

        [EnumValue("110112")]
        teCancSubst,

        [EnumValue("110113")]
        teEPEC,

        [EnumValue("110130")]
        teComprEntregaNFe,

        [EnumValue("110131")]
        teCancComprEntregaNFe,

        [EnumValue("110150")]
        teAtorInteressadoNFe,

        [EnumValue("210200")]
        teManifDestConfirmacao,

        [EnumValue("210210")]
        teManifDestCiencia,

        [EnumValue("210220")]
        teManifDestDesconhecimento,

        [EnumValue("210240")]
        teManifDestOperNaoRealizada,
    }
}