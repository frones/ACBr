using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public enum DeterminacaoBaseIcms
    {
        [EnumValue("")] dbiNenhum,

        [EnumValue("0")] dbiMargemValorAgregado,

        [EnumValue("1")] dbiPauta,

        [EnumValue("2")] dbiPrecoTabelado,

        [EnumValue("3")] dbiValorOperacao
    }
}