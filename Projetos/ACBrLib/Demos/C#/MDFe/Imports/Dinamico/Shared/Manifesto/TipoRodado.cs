using ACBrLib.Core;

namespace ACBrLib.MDFe
{
    public enum TipoRodado
    {
        [EnumValue("00")]
        trNaoAplicavel,

        [EnumValue("01")]
        trTruck,

        [EnumValue("02")]
        trToco,

        [EnumValue("03")]
        trCavaloMecanico,

        [EnumValue("04")]
        trVAN,

        [EnumValue("05")]
        trUtilitario,

        [EnumValue("06")]
        trOutros
    }
}