using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public enum IndIntermed
    {
        [EnumValue("")] iiSemOperacao,

        [EnumValue("0")] iiOperacaoSemIntermediador,

        [EnumValue("1")] iiOperacaoComIntermediador
    }
}