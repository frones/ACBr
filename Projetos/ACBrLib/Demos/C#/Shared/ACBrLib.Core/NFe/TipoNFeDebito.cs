namespace ACBrLib.Core.NFe
{
    public enum TipoNFeDebito
    {
        [EnumValue("")]
        tdNenhum,

        [EnumValue("01")]
        tdTransferenciaCreditoCooperativa = 01,

        [EnumValue("02")]
        tdAnulacao = 02,

        [EnumValue("03")]
        tdDebitosNaoProcessadas = 03,

        [EnumValue("04")]
        tdMultaJuros = 04,

        [EnumValue("05")]
        tdTransferenciaCreditoSucessao = 05
    }
}