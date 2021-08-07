using ACBrLib.Core;

namespace ACBrLib.MDFe
{
    public enum TipoCarroceria
    {
        [EnumValue("00")]
        tcNaoAplicavel,

        [EnumValue("01")]
        tcAberta,

        [EnumValue("02")]
        tcFechada,

        [EnumValue("03")]
        tcGraneleira,

        [EnumValue("04")]
        tcPortaContainer,

        [EnumValue("05")]
        tcSider
    }
}