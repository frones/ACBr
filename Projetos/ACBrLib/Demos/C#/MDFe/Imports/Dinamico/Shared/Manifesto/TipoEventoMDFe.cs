using ACBrLib.Core;

namespace ACBrLib.MDFe
{
    public enum TipoEventoMDFe
    {
        [EnumValue("-99999")]
        teNaoMapeado,

        [EnumValue("110111")]
        teCancelamento,

        [EnumValue("110112")]
        teEncerramento,

        [EnumValue("110114")]
        teInclusaoCondutor,

        [EnumValue("110115")]
        teInclusaoDFe,

        [EnumValue("110116")]
        tePagamentoOperacao,
    }
}