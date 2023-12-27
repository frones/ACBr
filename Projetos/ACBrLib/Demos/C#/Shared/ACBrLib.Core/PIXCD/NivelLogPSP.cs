using System.ComponentModel;

namespace ACBrLib.Core.PIXCD
{
    public enum NivelLogPSP
    {
        [Description("Nenhum")]
        logPSPNenhum = 0,

        [Description("Baixo")]
        logPSPBaixo = 1,

        [Description("Normal")]
        logPSPNormal = 2,

        [Description("Alto")]
        logPSPAlto = 3,

        [Description("Muito Alto")]
        logPSPMuitoAlto = 4,
    }
}