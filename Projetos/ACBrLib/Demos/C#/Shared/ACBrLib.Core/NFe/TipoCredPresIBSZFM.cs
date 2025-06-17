using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public enum TipoCredPresIBSZFM
    {
        [EnumValue("")] tcpNenhum,

        [EnumValue("0")] tcpSemCredito,

        [EnumValue("1")] tcpBensConsumoFinal,

        [EnumValue("2")] tcpBensCapital,

        [EnumValue("3")] tcpBensIntermediarios,

        [EnumValue("4")] tcpBensInformaticaOutros
    }
}
