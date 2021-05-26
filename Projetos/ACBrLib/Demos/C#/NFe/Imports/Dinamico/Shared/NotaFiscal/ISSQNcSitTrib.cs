using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public enum ISSQNcSitTrib
    {
        [EnumValue("")]
        ISSQNcSitTribVazio,

        [EnumValue("N")]
        ISSQNcSitTribNORMAL,

        [EnumValue("R")]
        ISSQNcSitTribRETIDA,

        [EnumValue("S")]
        ISSQNcSitTribSUBSTITUTA,

        [EnumValue("I")]
        ISSQNcSitTribISENTA
    }
}