namespace ACBrLib.Core.CTe
{
    public enum CSTCTe
    {
        [EnumValue("00")]
        tributacaoNormal = 00,

        [EnumValue("20")]
        tributacaoBCreduzidaICMS = 20,

        [EnumValue("40")]
        isencaoICMS = 40,

        [EnumValue("41")]
        ICMSNaoTributada = 41,

        [EnumValue("51")]
        ICMSDiferido = 51,

        [EnumValue("60")]
        ICMSCobradoSubstituicaoTributaria = 60,

        [EnumValue("90")]
        ICMSOutros = 90,

        [EnumValue("90")]
        ICMSOutraUF = 90,

        [EnumValue("90")]
        ICMSSN = 90,
    }
}