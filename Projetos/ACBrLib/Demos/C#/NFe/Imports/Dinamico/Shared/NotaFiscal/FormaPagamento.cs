using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public enum FormaPagamento
    {
        [EnumValue("01")]
        fpDinheiro,

        [EnumValue("02")]
        fpCheque,

        [EnumValue("03")]
        fpCartaoCredito,

        [EnumValue("04")]
        fpCartaoDebito,

        [EnumValue("05")]
        fpCreditoLoja,

        [EnumValue("10")]
        fpValeAlimentacao,

        [EnumValue("11")]
        fpValeRefeicao,

        [EnumValue("12")]
        fpValePresente,

        [EnumValue("13")]
        fpValeCombustivel,

        [EnumValue("14")]
        fpDuplicataMercantil,

        [EnumValue("15")]
        fpBoletoBancario,

        [EnumValue("16")]
        fpDepositoBancario,

        [EnumValue("17")]
        fpPagamentoInstantaneo,

        [EnumValue("18")]
        fpTransfBancario,

        [EnumValue("19")]
        fpProgramaFidelidade,

        [EnumValue("20")]
        fpPagamentoInstantaneoEstatico,

        [EnumValue("21")]
        fpCreditoEmLojaPorDevolucao,

        [EnumValue("22")]
        fpFalhaHardware,

        [EnumValue("90")]
        fpSemPagamento,

        [EnumValue("98")]
        fpRegimeEspecial,

        [EnumValue("99")]
        fpOutro
    }
}