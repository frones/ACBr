using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public enum IndicadorPagamento
    {
        [EnumValue("")]
        ipNenhum,

        [EnumValue("0")]
        ipVista,

        [EnumValue("1")]
        ipPrazo,

        [EnumValue("2")]
        ipOutras
    }
}