using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public enum IndIntermed
    {
        [EnumValue("")] iiSemOperacao,

        [EnumValue("0")] iiOperacaoSemIntermediador = 1,

        [EnumValue("1")] iiOperacaoComIntermediador = 2
    }
}