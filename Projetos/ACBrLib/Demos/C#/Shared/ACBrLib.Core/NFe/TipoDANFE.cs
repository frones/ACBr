using System.ComponentModel;

namespace ACBrLib.Core.NFe
{
    public enum TipoDANFE
    {
        [Description("Sem Impressão")]
        tiSemGeracao = 0,

        [Description("Retrato")]
        tiRetrato = 1,

        [Description("Paisagem")]
        tiPaisagem = 2,

        [Description("Simplificado")]
        tiSimplificado = 3,

        [Description("NFCe")]
        tiNFCe = 4,

        [Description("NFCe Digital")]
        tiMsgEletronica = 5
    }
}