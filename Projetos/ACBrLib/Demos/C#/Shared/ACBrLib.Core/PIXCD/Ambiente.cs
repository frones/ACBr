using System.ComponentModel;

namespace ACBrLib.Core.PIXCD
{
    public enum Ambiente
    {
        [Description("Teste")]
        ambTeste = 0,

        [Description("Produção")]
        ambProducao = 1,

        [Description("Pré-Produção")]
        ambPreProducao = 2,
    }
}